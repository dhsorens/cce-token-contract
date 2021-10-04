(* This is the orderbook exchange that services the carbon contract *)
(* The structure is a double-sided auction *)

(* =============================================================================
 * Storage
 * ============================================================================= *)

type token = { token_address : address ; token_id : nat ; }

type token_for_sale = {
    owner : address ; 
    token_address : address ; 
    token_id : nat ; 
    qty : nat ;
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

type auction_data = { 
    leader : address ; 
    leading_bid : nat ;
    deadline : timestamp ; // the end of the auction
    reserve_price : nat ; // in mutez
}

type storage = {
    carbon_contract : address ;
    tokens_for_sale : (token_for_sale, sale_data) big_map ;
    tokens_on_auction : (token_for_sale, auction_data) big_map ;
    offers : (token_offer, offer_data) big_map ;
    token_whitelist : (token, unit) big_map ; 
}
// TODO : collisions

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
| InitiateAuction of token_for_sale * auction_data
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
    if (token.owner <> Tezos.sender) then (failwith error_PERMISSIONS_DENIED : result) else

    // check the token is whitelisted
    let token_data : token = { token_address = token.token_address ; token_id = token.token_id ; } in 
    let () = (match (Big_map.find_opt token_data storage.token_whitelist) with 
              | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) 
              | Some u -> u) in 

    // receive the tokens; sender has to authorize this as an operator
    let txndata_receive_tokens = 
        (Tezos.sender, [ (Tezos.self_address, token.token_id, token.qty) ; ]) in
    let entrypoint_receive_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_tokens = 
        Tezos.transaction txndata_receive_tokens 0tez entrypoint_receive_tokens in 
    
    // update storage 
    let updated_tokens_for_sale = Big_map.update token (Some data) storage.tokens_for_sale in 

    [op_receive_tokens], 
    { storage with tokens_for_sale = updated_tokens_for_sale ; }


let unpost_for_sale (token : token_for_sale) (storage : storage) : result = 
    // check permissions
    if (token.owner <> Tezos.sender) then (failwith error_PERMISSIONS_DENIED : result) else

    // check the token is actually for sale; if it is, remove it from storage.tokens_for_sale
    let (_, updated_tokens_for_sale) : sale_data * (token_for_sale, sale_data) big_map = (
        match (Big_map.get_and_update token (None : sale_data option) storage.tokens_for_sale : sale_data option * (token_for_sale, sale_data) big_map) with
        | (None, _) -> (failwith error_TOKEN_FOR_SALE_NOT_FOUND : sale_data * (token_for_sale, sale_data) big_map)
        | (Some d, s) -> (d, s)
    ) in

    // send the token back 
    let txndata_return_tokens = 
        (Tezos.self_address, [ (token.owner, token.token_id, token.qty) ; ]) in 
    let entrypoint_return_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_return_tokens = 
        Tezos.transaction txndata_return_tokens 0tez entrypoint_return_tokens in 

    [op_return_tokens], 
    {storage with tokens_for_sale = updated_tokens_for_sale ;}


let buy_for_sale (token: token_for_sale) (storage : storage) : result = 
    let buyer = Tezos.sender in 

    // verify token is for sale and buyer has sent enough xtz
    // if everything checks out, update the storage
    let (price, updated_tokens_for_sale) : nat * (token_for_sale, sale_data) big_map = (
        match (Big_map.get_and_update token (None : sale_data option) storage.tokens_for_sale : sale_data option * (token_for_sale, sale_data) big_map) with
        | (None, _) -> (failwith error_TOKEN_FOR_SALE_NOT_FOUND : nat * (token_for_sale, sale_data) big_map) 
        | (Some s, u) -> (s.price, u)
    ) in 
    if (Tezos.amount < (price * 1mutez)) then (failwith error_INSUFFICIENT_FUNDS : result) else

    // send the tokens to the buyer
    let txndata_send_tokens = 
        (Tezos.self_address, [ (buyer, token.token_id, token.qty) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_tokens = 
        Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 

    // send the XTZ along to the seller (owner)
    let entrypoint_pay_seller = (
        match (Tezos.get_contract_opt token.owner : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some e -> e
    ) in 
    let op_pay_seller = Tezos.transaction () (price * 1mutez) entrypoint_pay_seller in 

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
let initiate_auction (token, data : token_for_sale * auction_data) (storage : storage) : result = 
    // check the deadline is not already passed
    if (data.deadline <= Tezos.now) then (failwith error_INVALID_DEADLINE : result) else
    
    // receive the tokens
    let txndata_receive_tokens = 
        (Tezos.sender, [ (Tezos.self_address, token.token_id, token.qty) ; ]) in
    let entrypoint_receive_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_tokens = 
        Tezos.transaction txndata_receive_tokens 0tez entrypoint_receive_tokens in 

    // update the tokens_on_auction big map
    let init_data : auction_data = {
        leader = Tezos.sender ; 
        leading_bid = data.reserve_price ;
        deadline = data.deadline ;
        reserve_price = data.reserve_price ;
    } in
    
    let new_tokens_on_auction = 
        Big_map.update token (Some init_data) storage.tokens_on_auction in 

    [ op_receive_tokens ; ],
    { storage with tokens_on_auction = new_tokens_on_auction ; }


let bid_on_auction (token : token_for_sale) (storage : storage) : result = 
    // get auction data
    let data = (
        match (Big_map.find_opt token storage.tokens_on_auction : auction_data option) with
        | None -> (failwith error_AUCTIONED_TOKEN_NOT_FOUND : auction_data)
        | Some d -> d
    ) in 
    let leader = data.leader in 
    let leading_bid = data.leading_bid in 

    // check the deadline is not past
    if (data.deadline <= Tezos.now) then (failwith error_AUCTION_IS_OVER : result) else 

    // if the bid isn't at least 0.1tez higher than the leading bid, the transaction fails
    let bid = Tezos.amount in 
    if (bid <= (leading_bid * 1mutez) + 100_000mutez) then (failwith error_BID_TOO_LOW : result) else 

    // update the storage to include the new leader
    let new_data = { data with leader = Tezos.sender ; leading_bid = (bid / 1mutez) ; } in 
    let new_tokens_on_auction = Big_map.update token (Some new_data) storage.tokens_on_auction in

    // if the bid is higher than the leader's bid, return the leader's cash
    if (leader = token.owner && leading_bid = data.reserve_price) // no one has bid
    then
        ([] : operation list),
        { storage with tokens_on_auction = new_tokens_on_auction ; }
    else 
        let entrypoint_returnOldBid : unit contract = (
            match (Tezos.get_contract_opt leader : unit contract option) with 
            | None -> (failwith error_INVALID_ADDRESS : unit contract)
            | Some e -> e
        ) in
        let op_returnOldBid = Tezos.transaction () (leading_bid * 1mutez) entrypoint_returnOldBid in 

        [ op_returnOldBid ; ], 
        { storage with tokens_on_auction = new_tokens_on_auction ; }


let finish_auction (token : token_for_sale) (storage : storage) : result = 
    // send tokens to the leader
    let data = (
        match (Big_map.find_opt token storage.tokens_on_auction : auction_data option) with
        | None -> (failwith error_AUCTIONED_TOKEN_NOT_FOUND : auction_data)
        | Some d -> d
    ) in 
    let leader = data.leader in 
    let leading_bid = data.leading_bid in // in mutez

    // check the deadline has not passed
    if (data.deadline > Tezos.now) then (failwith error_AUCTION_NOT_OVER : result) else

    // transfer tokens to the leader
    let txndata_send_tokens = 
        (Tezos.self_address, [ (leader, token.token_id, token.qty) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_tokens = 
        Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 

    // remove this token from storage 
    let new_tokens_on_auction = Big_map.update token (None : auction_data option) storage.tokens_on_auction in  

    // transfer highest bid to the owner
    if (leader = token.owner && leading_bid = data.reserve_price) // no one ever bid
    then 
        [ op_send_tokens ; ],
        { storage with tokens_on_auction = new_tokens_on_auction ; }
    else 
        // send the highest bid along to token.owner
        let entrypoint_payout : unit contract = (
            match (Tezos.get_contract_opt token.owner : unit contract option) with
            | None -> (failwith error_INVALID_ADDRESS : unit contract)
            | Some e -> e
        ) in
        let op_payout = Tezos.transaction () (leading_bid * 1mutez) entrypoint_payout in 
        
        [ op_send_tokens ; op_payout ; ],
        { storage with tokens_on_auction = new_tokens_on_auction ; }

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
    // the offer-maker sends their offer in the txn
    let quote = (Tezos.amount / 1mutez) in 
    let offering_party = Tezos.sender in 

    // compile data
    let token_offer : token_offer = {
        owner = token.owner ;
        token_address = token.token_address ; 
        token_id = token.token_id ;
        qty = token.qty ;
        offering_party = offering_party ;
    } in
    let data : offer_data = {
        quote = quote ;
    } in
   
    let updated_offers = (
        match (Big_map.find_opt token_offer storage.offers : offer_data option) with 
        | Some o -> (failwith error_OFFER_ALREADY_MADE : (token_offer, offer_data) big_map)
        | None -> (
            Big_map.update token_offer (Some data) storage.offers 
        )
    ) in

    ([] : operation list), 
    { storage with offers = updated_offers ; }

let retract_offer (token : token_for_sale) (storage : storage) : result = 
    // the offer-maker's data
    let token_offer : token_offer = {
        owner = token.owner ;
        token_address = token.token_address ; 
        token_id = token.token_id ;
        qty = token.qty ;
        offering_party = Tezos.sender ;
    } in

    // if no offer exists, nothing gets transferred 
    let (quote, new_offers) : nat * (token_offer, offer_data) big_map = (
        match (Big_map.get_and_update token_offer (None : offer_data option) storage.offers : offer_data option * (token_offer, offer_data) big_map) with
        | (None, o) -> (0n, o)
        | (Some q, o) -> (q.quote, o)
    ) in 

    // return the offering party's funds
    let entrypoint_return : unit contract = (
        match (Tezos.get_contract_opt Tezos.sender : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some c -> c
    ) in 
    let op_return = Tezos.transaction () (quote * 1mutez) entrypoint_return in 

    [ op_return ; ],
    { storage with offers = new_offers ; }


let accept_offer (token, data : token_offer * offer_data) (storage : storage) : result = 
    // make sure offer is as expected to prevent frontrunning attacks
    let (offer, new_offers) : offer_data * (token_offer, offer_data) big_map = (
        match (Big_map.get_and_update token (None : offer_data option) storage.offers : offer_data option * (token_offer, offer_data) big_map) with
        | (None, _) -> (failwith error_NO_OFFER_FOUND : offer_data * (token_offer, offer_data) big_map)
        | (Some o, n) -> (o, n)
    ) in 
    if (offer.quote <> data.quote) then (failwith error_NO_OFFER_FOUND : result) else

    // transfer the tokens to the offering party
    let token_data : token_for_sale = {
        owner = token.owner ;
        token_address = token.token_address ;
        token_id = token.token_id ;
        qty = token.qty ;
    } in
    let buyer = token.offering_party in 
    
    let txndata_send_tokens = 
        (Tezos.self_address, [ (buyer, token.token_id, token.qty) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_tokens = 
        Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 
    
    // transfer the XTZ of the offer to the owner 
    let entrypoint_send_xtz : unit contract = (
        match (Tezos.get_contract_opt token.owner : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some c -> c
    ) in 
    let op_send_xtz = Tezos.transaction () (offer.quote * 1mutez) entrypoint_send_xtz in 

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
 WhitelistTokens Entrypoint Functions 
 *** **)
let rec whitelist_tokens (param, storage : whitelist_tokens * storage) : result = 
    if (Tezos.sender <> storage.carbon_contract) then (failwith error_PERMISSIONS_DENIED : result) else 

    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl ->
        let (token, add_or_remove) = hd in 
        let updated_whitelist : (token, unit) big_map = 
            Big_map.update token add_or_remove storage.token_whitelist in

        whitelist_tokens (tl, {storage with token_whitelist = updated_whitelist ;})


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
    | WhitelistTokens param -> // the tokens on the market
        whitelist_tokens (param, storage)