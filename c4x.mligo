(* This is the orderbook exchange that services the carbon contract *)
(* The structure is a double-sided auction *)
#include "aux/carbon-fa2-types.mligo"

(* =============================================================================
 * Storage
 * ============================================================================= *)

type token = { token_address : address ; token_id : nat ; }

type token_for_sale = {
    owner : address ; 
    token_address : address ; 
    token_id : nat ; 
    qty : nat ;
    batch_number : nat ; // to avoid collisions; default is 0n
}
type sale_data = { price : nat ; }

type token_offer = {
    owner : address ; 
    token_address : address ; 
    token_id : nat ; 
    qty : nat ;
    offering_party : address ; 
}
type offer_data = { quote : nat ; }

type init_auction_data = {
    deadline : timestamp ; // the end of an auction
    reserve_price : nat ; // in mutez
}
type auction_data = { 
    leader : address ; 
    leading_bid : nat ; // the leader's bid 
    deadline : timestamp ; // the end of the auction
    reserve_price : nat ; // in mutez
}

type redeemable_token = {
    token_address : address ; 
    token_id : nat ; 
    qty : nat ;
}
type redeemable = 
| Tokens of redeemable_token 
| XTZ of nat 

type storage = {
    carbon_contract : address ;
    tokens_for_sale : (token_for_sale, sale_data) big_map ;
    tokens_on_auction : (token_for_sale, auction_data) big_map ;
    offers : (token_offer, offer_data) big_map ;
    token_whitelist : (token, unit) big_map ; 
    redeem : (address, redeemable list) big_map ;
    null_address : address ;
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type buy_for_sale = { buyer : address ; token : token ; owner : address ; amt : nat ; }
type whitelist_tokens = (token * (unit option)) list

type for_sale = 
| PostForSale   of token_for_sale * sale_data
| UnpostForSale of token_for_sale
| BuyForSale    of token_for_sale

type auction = 
| InitiateAuction of token_for_sale * init_auction_data
| BidOnAuction    of token_for_sale
| FinishAuction   of token_for_sale

type offer = 
| MakeOffer    of token_for_sale
| RetractOffer of token_for_sale
| AcceptOffer  of token_offer    * offer_data

type entrypoint = 
| ForSale of for_sale // a seller posts their tokens for sale at a given price
| Auction of auction  // a seller auctions off their tokens
| Offer of offer // a buyer makes an offer for tokens
| Redeem of unit // redeem tokens and xtz for the sender
| WhitelistTokens of whitelist_tokens // updated by the carbon contract

type result = operation list * storage

(* =============================================================================
 * ERROR CODES
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n 
let error_TOKEN_NOT_WHITELISTED = 1n 
let error_NO_TOKEN_CONTRACT_FOUND = 2n 
let error_INVALID_ADDRESS = 3n
let error_TOKEN_FOR_SALE_NOT_FOUND = 4n
let error_AUCTION_IS_OVER = 5n
let error_AUCTION_NOT_OVER = 6n
let error_AUCTIONED_TOKEN_NOT_FOUND = 7n
let error_BID_TOO_LOW = 8n
let error_INVALID_DEADLINE = 9n
let error_OFFER_ALREADY_MADE = 10n
let error_NO_OFFER_FOUND = 11n
let error_INSUFFICIENT_FUNDS = 12n
let error_COLLISION = 13n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)




(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

(*** ** 
 ForSale Entrypoint Functions 
 *** **)
let post_for_sale (token, data : token_for_sale * sale_data) (storage : storage) : result = 
    // check permissions and collisions
    if token.owner <> Tezos.sender then (failwith error_PERMISSIONS_DENIED : result) else
    if Big_map.mem token storage.tokens_for_sale then (failwith error_COLLISION : result) else
    // check the token is whitelisted
    let token_data : token = { token_address = token.token_address ; token_id = token.token_id ; } in 
    if not Big_map.mem token_data storage.token_whitelist then (failwith error_TOKEN_NOT_WHITELISTED : result) else
    // receive the tokens; sender has to authorize this as an operator
    let txndata_receive_tokens = { 
        from_ = Tezos.sender ; 
        txs = [ { to_ = Tezos.self_address ; token_id = token.token_id ; amount = token.qty ; } ; ] ; } in
    let entrypoint_receive_tokens =
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : transfer list contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
        | Some e -> e in
    let op_receive_tokens = 
        Tezos.transaction [txndata_receive_tokens] 0tez entrypoint_receive_tokens in 
    // update storage 
    let tokens_for_sale = Big_map.update token (Some data) storage.tokens_for_sale in 
    // output
    [op_receive_tokens], 
    { storage with tokens_for_sale = tokens_for_sale ; }


let unpost_for_sale (token : token_for_sale) (storage : storage) : result = 
    // check permissions
    if token.owner <> Tezos.sender then (failwith error_PERMISSIONS_DENIED : result) else
    // check the token is actually for sale; if it is, remove it from storage.tokens_for_sale
    let (_, updated_tokens_for_sale) : sale_data * (token_for_sale, sale_data) big_map =
        match Big_map.get_and_update token (None : sale_data option) storage.tokens_for_sale with
        | (None, _) -> (failwith error_TOKEN_FOR_SALE_NOT_FOUND : sale_data * (token_for_sale, sale_data) big_map)
        | (Some d, s) -> (d, s) in
    // send the token back 
    let txndata_return_tokens = { 
        from_ = Tezos.self_address ; 
        txs = [ { to_ = token.owner ; token_id = token.token_id ; amount = token.qty ; } ; ] ; } in
    let entrypoint_return_tokens =
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : transfer list contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
        | Some e -> e in 
    let op_return_tokens = 
        Tezos.transaction [txndata_return_tokens] 0tez entrypoint_return_tokens in 
    // output
    [op_return_tokens], 
    {storage with tokens_for_sale = updated_tokens_for_sale ;}


let buy_for_sale (token: token_for_sale) (storage : storage) : result = 
    let buyer = Tezos.sender in 
    // verify token is for sale and buyer has sent enough xtz
    // if everything checks out, update the storage
    let (price, updated_tokens_for_sale) : nat * (token_for_sale, sale_data) big_map =
        match Big_map.get_and_update token (None : sale_data option) storage.tokens_for_sale with
        | (None, _) -> (failwith error_TOKEN_FOR_SALE_NOT_FOUND : nat * (token_for_sale, sale_data) big_map) 
        | (Some s, u) -> (s.price, u) in 
    if Tezos.amount < price * 1mutez then (failwith error_INSUFFICIENT_FUNDS : result) else
    // send the tokens to the buyer
    let txndata_send_tokens = {
        from_ = Tezos.self_address ; 
        txs = [ { to_ = buyer ; token_id = token.token_id ; amount = token.qty ; } ; ] ; } in 
    let entrypoint_send_tokens =
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : transfer list contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
        | Some e -> e in 
    let op_send_tokens = 
        Tezos.transaction [txndata_send_tokens] 0tez entrypoint_send_tokens in 
    // send the XTZ along to the seller (owner)
    let entrypoint_pay_seller =
        match (Tezos.get_contract_opt token.owner : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some e -> e in 
    let op_pay_seller = Tezos.transaction () (price * 1mutez) entrypoint_pay_seller in 
    // output
    [op_send_tokens ; op_pay_seller ;], 
    {storage with tokens_for_sale = updated_tokens_for_sale ;}

let for_sale (param : for_sale) (storage : storage) : result = 
    match param with
    | PostForSale p -> 
        post_for_sale p storage
    | UnpostForSale p ->
        unpost_for_sale p storage
    | BuyForSale p ->  
        buy_for_sale p storage


(*** **
 Auction Entrypoint Functions 
 *** **)
let initiate_auction (token, data : token_for_sale * init_auction_data) (storage : storage) : result = 
    // check the deadline is not already passed, for collisions, and that the token is whitelisted
    if data.deadline <= Tezos.now then (failwith error_INVALID_DEADLINE : result) else
    if Big_map.mem token storage.tokens_on_auction then (failwith error_COLLISION : result) else
    if not Big_map.mem {token_address = token.token_address ; token_id = token.token_id ; } storage.token_whitelist then (failwith error_TOKEN_NOT_WHITELISTED : result) else
    // receive the tokens
    let txndata_receive_tokens = { 
        from_ = Tezos.sender ; 
        txs = [ { to_ = Tezos.self_address ; token_id = token.token_id ; amount = token.qty ; } ; ] ; } in
    let entrypoint_receive_tokens =
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : transfer list contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
        | Some e -> e in
    let op_receive_tokens = 
        Tezos.transaction [txndata_receive_tokens] 0tez entrypoint_receive_tokens in 
    // update the tokens_on_auction big map
    let init_data : auction_data = {
        leader = Tezos.sender ; 
        leading_bid = 0n ;
        deadline = data.deadline ;
        reserve_price = data.reserve_price ; } in
    // output
    [ op_receive_tokens ; ],
    { storage with tokens_on_auction = Big_map.update token (Some init_data) storage.tokens_on_auction ; }


let bid_on_auction (token : token_for_sale) (storage : storage) : result = 
    // get auction data
    let data =
        match (Big_map.find_opt token storage.tokens_on_auction : auction_data option) with
        | None -> (failwith error_AUCTIONED_TOKEN_NOT_FOUND : auction_data)
        | Some d -> d in 
    // check the deadline is not past and that the bidding party is not the owner
    if data.deadline <= Tezos.now then (failwith error_AUCTION_IS_OVER : result) else 
    if Tezos.sender = token.owner then (failwith error_PERMISSIONS_DENIED : result) else
    // check the bid is sufficiently high
    let bid = Tezos.amount in 
    let no_one_has_bid = (data.leader = token.owner) in 
    // if no one has bid, just make sure the bid is at least the reserve price
    if no_one_has_bid && bid < data.reserve_price * 1mutez 
        then (failwith error_BID_TOO_LOW : result) else
    // if someone else has already bid, then a new bid must go up by at least 1%
    if (not no_one_has_bid) && bid < (data.leading_bid * 1mutez * 101n) / 100n then (failwith error_BID_TOO_LOW : result) else 
    // update the storage to include the new leader
    let tokens_on_auction = 
        Big_map.update 
        token 
        (Some { data with 
            leader = Tezos.sender ; 
            leading_bid = bid / 1mutez ; 
            // add five mins to the deadline for bids made in the last five mins to prevent sniping
            deadline = if data.deadline - Tezos.now < 300 then data.deadline + 300 else data.deadline ; })
        storage.tokens_on_auction in
    // if the bid is higher than the leader's bid, return the leader's cash
    if no_one_has_bid
    then
        ([] : operation list),
        { storage with tokens_on_auction = tokens_on_auction ; }
    else 
        // return the old bid by making it redeemable 
        // if you don't do this and try to include the return in this transaction,
        // a contract that can't receive funds could win all auctions by making it impossible
        // to place a bid above them.
        let redeem = 
            let old_redeemable = match Big_map.find_opt data.leader storage.redeem with 
            | None -> ([] : redeemable list)
            | Some l -> l in 
            let updated_redeemable = XTZ(data.leading_bid) :: old_redeemable in 
            Big_map.update data.leader (Some updated_redeemable) storage.redeem in 

        ([] : operation list), 
        { storage with 
            tokens_on_auction = tokens_on_auction ; 
            redeem = redeem ; }

// this function makes the auctioned tokens and the leading bid redeemable by 
// the leader and token owner, respectively 
let finish_auction (token : token_for_sale) (storage : storage) : result = 
    let data = match Big_map.find_opt token storage.tokens_on_auction with
    | None -> (failwith error_AUCTIONED_TOKEN_NOT_FOUND : auction_data)
    | Some d -> d in 
    // check the deadline has passed
    if Tezos.now < data.deadline then (failwith error_AUCTION_NOT_OVER : result) else 
    if Tezos.sender <> data.leader && Tezos.sender <> token.owner then (failwith error_PERMISSIONS_DENIED : result) else
    // make the tokens on auction redeemable
    let redeemable_tokens = { token_address = token.token_address ; token_id = token.token_id ; qty = token.qty ; } in 
    let redeem = 
        let old_redeemable = match Big_map.find_opt data.leader storage.redeem with
        | None -> ([] : redeemable list)
        | Some l -> l in 
        let updated_redeemable = Tokens(redeemable_tokens) :: old_redeemable in 
        Big_map.update data.leader (Some updated_redeemable) storage.redeem in 
    // make the highest bid redeemable
    let redeem = 
        let old_redeemable = match Big_map.find_opt token.owner redeem with
        | None -> ([] : redeemable list)
        | Some l -> l in 
        let updated_redeemable = XTZ(data.leading_bid) :: old_redeemable in 
        Big_map.update token.owner (Some updated_redeemable) redeem in 
    // remove the auction from storage 
    let tokens_on_auction = Big_map.update token (None : auction_data option) storage.tokens_on_auction in 
    ([] : operation list),
    { storage with 
        tokens_on_auction = tokens_on_auction ;
        redeem = redeem ; }


let auction (param : auction) (storage : storage) : result = 
    match param with 
    | InitiateAuction p ->
        initiate_auction p storage
    | BidOnAuction p ->
        bid_on_auction p storage 
    | FinishAuction p -> 
        finish_auction p storage

(*** **
 Offer Entrypoint Functions 
 *** **) 
let make_offer (token : token_for_sale) (storage : storage) : result = 
    // make sure the token is whitelisted
    if not Big_map.mem { token_address = token.token_address ; token_id = token.token_id ; } storage.token_whitelist then (failwith error_TOKEN_NOT_WHITELISTED : result) else
    // the offer-maker sends their offer in the txn
    let quote = (Tezos.amount / 1mutez) in 
    let offering_party = Tezos.sender in 
    // collect data
    let token_offer : token_offer = {
        owner = token.owner ;
        token_address = token.token_address ; 
        token_id = token.token_id ;
        qty = token.qty ;
        offering_party = offering_party ; } in
    let data : offer_data = {
        quote = quote ; } in
    // update offers in storage
    let offers =
        match (Big_map.find_opt token_offer storage.offers : offer_data option) with 
        | Some _ -> (failwith error_OFFER_ALREADY_MADE : (token_offer, offer_data) big_map)
        | None -> Big_map.update token_offer (Some data) storage.offers in
    // output
    ([] : operation list), 
    { storage with offers = offers ; }

let retract_offer (token : token_for_sale) (storage : storage) : result = 
    // the offer-maker's data
    let token_offer : token_offer = {
        owner = token.owner ;
        token_address = token.token_address ; 
        token_id = token.token_id ;
        qty = token.qty ;
        offering_party = Tezos.sender ; } in
    // if no offer exists, nothing gets transferred 
    let (quote, new_offers) : nat * (token_offer, offer_data) big_map = 
        match (Big_map.get_and_update token_offer (None : offer_data option) storage.offers : offer_data option * (token_offer, offer_data) big_map) with
        | (None, o) -> (0n, o)
        | (Some q, o) -> (q.quote, o) in 
    // return the offering party's funds
    let entrypoint_return : unit contract =
        match (Tezos.get_contract_opt Tezos.sender : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some c -> c in 
    let op_return = Tezos.transaction () (quote * 1mutez) entrypoint_return in 
    // output
    [ op_return ; ],
    { storage with offers = new_offers ; }


let accept_offer (token, data : token_offer * offer_data) (storage : storage) : result = 
    // make sure offer is as expected to prevent frontrunning attacks
    let (offer, new_offers) : offer_data * (token_offer, offer_data) big_map =
        match (Big_map.get_and_update token (None : offer_data option) storage.offers : offer_data option * (token_offer, offer_data) big_map) with
        | (None, _) -> (failwith error_NO_OFFER_FOUND : offer_data * (token_offer, offer_data) big_map)
        | (Some o, n) -> (o, n) in 
    if offer.quote <> data.quote then (failwith error_NO_OFFER_FOUND : result) else
    // transfer the tokens to the offering party
    let buyer = token.offering_party in 
    let txndata_send_tokens = {
        from_ = token.owner ; 
        txs = [ { to_ = buyer ; token_id = token.token_id ; amount = token.qty ; } ; ] ; } in 
    let entrypoint_send_tokens =
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : transfer list contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
        | Some e -> e in 
    let op_send_tokens = 
        Tezos.transaction [txndata_send_tokens] 0tez entrypoint_send_tokens in 
    // transfer the XTZ of the offer to the owner 
    let entrypoint_send_xtz : unit contract = 
        match (Tezos.get_contract_opt token.owner : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some c -> c in 
    let op_send_xtz = Tezos.transaction () (offer.quote * 1mutez) entrypoint_send_xtz in 
    // output
    [ op_send_tokens ; op_send_xtz ; ],
    { storage with offers = new_offers ; }

let offer (param : offer) (storage : storage) : result = 
    match param with 
    | MakeOffer p ->
        make_offer p storage
    | RetractOffer p ->
        retract_offer p storage
    | AcceptOffer p ->
        accept_offer p storage


(*** ** 
 Redeem Entrypoint Function
 *** **)

let rec redeem_assets (send_to, list_to_redeem, acc : address * (redeemable list) * (operation list)) : operation list = 
    match list_to_redeem with 
    | [] -> acc 
    | hd :: tl -> (
        match hd with 
        | Tokens t -> 
            // transfer tokens to send_to
            let txndata_send_tokens = {
                from_ = Tezos.self_address ; 
                txs = [ { to_ = send_to ; token_id = t.token_id ; amount = t.qty ; } ; ] ; } in 
            let entrypoint_send_tokens =
                match (Tezos.get_entrypoint_opt "%transfer" t.token_address : transfer list contract option) with 
                | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : transfer list contract)
                | Some e -> e in 
            let acc = (Tezos.transaction [txndata_send_tokens] 0tez entrypoint_send_tokens) :: acc in 
            redeem_assets(send_to, tl, acc)
        | XTZ amt -> 
            // transfer amt worth of XTZ to this address 
            let entrypoint_transfer = match (Tezos.get_contract_opt send_to : unit contract option) with
            | None -> (failwith error_INVALID_ADDRESS : unit contract)
            | Some c -> c in 
            let acc = (Tezos.transaction () (amt * 1mutez) entrypoint_transfer) :: acc in 
            redeem_assets(send_to, tl, acc))


let redeem (_ : unit) (storage : storage) : result = 
    let send_to = Tezos.sender in 
    // fetch their redeemable funds, make operations to send them, and send them
    let list_to_redeem = match Big_map.find_opt send_to storage.redeem with 
        | None -> ([] : redeemable list)
        | Some l -> l in 
    let ops = redeem_assets (send_to, list_to_redeem, ([] : operation list)) in 
    // remove the redeemed assets from storage 
    let redeem = Big_map.update send_to (None : redeemable list option) storage.redeem in 
    ops, {storage with redeem = redeem ;}


(*** ** 
 WhitelistTokens Entrypoint Functions 
 *** **)
let rec whitelist_tokens (param, storage : whitelist_tokens * storage) : result = 
    if Tezos.sender <> storage.carbon_contract then (failwith error_PERMISSIONS_DENIED : result) else 
    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl ->
        let (token, add_or_remove) = hd in 
        let token_whitelist : (token, unit) big_map = 
            Big_map.update token add_or_remove storage.token_whitelist in
        whitelist_tokens (tl, {storage with token_whitelist = token_whitelist ;})


(* =============================================================================
 * Main
 * ============================================================================= *)

let main (entrypoint, storage : entrypoint * storage) = 
    match entrypoint with 
    | ForSale param -> // a seller posts their tokens for sale at a given price
        for_sale param storage
    | Auction param -> // a seller auctions off their tokens
        auction param storage
    | Offer param -> // a buyer makes an offer for some tokens
        offer param storage
    | Redeem param -> 
        redeem param storage
    | WhitelistTokens param -> // the tokens on the market
        whitelist_tokens (param, storage)