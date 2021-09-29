(* This is the orderbook exchange that services the carbon contract *)

(* =============================================================================
 * Storage
 * ============================================================================= *)

type token = { token_address : address ; token_id : nat ; }
type selling = { owner : address ; amt : nat ; }

type storage = {
    carbon_contract : address ;
    tokens_for_sale : (token * selling, unit) big_map ; 
    token_whitelist : (token, unit) big_map ; 
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type post_for_sale = { token : token ; owner : address ; amt : nat ; }
type buy_for_sale = { buyer : address ; token : token ; owner : address ; amt : nat ; }
type whitelist_tokens = (token * (unit option)) list

type entrypoint = 
| PostForSale of post_for_sale 
| UnpostForSale of post_for_sale
| BuyForSale of buy_for_sale
| WhitelistTokens of whitelist_tokens

type result = operation list * storage

(* =============================================================================
 * ERROR CODES
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n 
let error_TOKEN_NOT_WHITELISTED = 1n 
let error_NO_TOKEN_CONTRACT_FOUND = 2n 
let error_INVALID_ADDRESS = 3n
let error_NO_SUCH_ENTRY = 4n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)




(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let post_for_sale (param : post_for_sale) (storage : storage) : result = 
    if (param.owner <> Tezos.sender) then (failwith error_PERMISSIONS_DENIED : result) else

    let token = param.token in 
    let owner = param.owner in 
    let amt = param.amt in 

    // check the token is whitelisted
    let () = (match (Big_map.find_opt token storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 

    // receive the tokens
    let txndata_receive_tokens = 
        (Tezos.sender, [ (Tezos.self_address, token.token_id, amt) ; ]) in
    let entrypoint_receive_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_tokens = 
        Tezos.transaction txndata_receive_tokens 0tez entrypoint_receive_tokens in 
    
    // update storage 
    let new_entry = (token, { owner = owner ; amt = amt ;}) in 
    let updated_tokens_for_sale = Big_map.update new_entry (Some ()) storage.tokens_for_sale in 

    [op_receive_tokens], {storage with tokens_for_sale = updated_tokens_for_sale ;}


let unpost_for_sale (param : post_for_sale) (storage : storage) : result = 
    if (param.owner <> Tezos.sender) then (failwith error_PERMISSIONS_DENIED : result) else

    let token = param.token in 
    let owner = param.owner in 
    let amt = param.amt in 

    // check 
    let seller_data = (token, { owner = owner ; amt = amt ;}) in 
    let () = (
        match (Big_map.find_opt seller_data storage.tokens_for_sale) with
        | None -> (failwith error_NO_SUCH_ENTRY : unit)
        | Some u -> u
    ) in 

    // send the token back 
    let txndata_return_tokens = 
        (Tezos.self_address, [ (Tezos.sender, token.token_id, amt) ; ]) in 
    let entrypoint_return_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_return_tokens = 
        Tezos.transaction txndata_return_tokens 0tez entrypoint_return_tokens in 

    // update storage
    let updated_tokens_for_sale = Big_map.update seller_data (None : unit option) storage.tokens_for_sale in 

    [op_return_tokens], {storage with tokens_for_sale = updated_tokens_for_sale ;}


let buy_for_sale (param : buy_for_sale) (storage : storage) : result = 
    if (param.buyer <> Tezos.sender) then (failwith error_PERMISSIONS_DENIED : result) else

    let buyer = param.buyer in 
    let token = param.token in 
    let seller = param.owner in 
    let amt = param.amt in 

    // check there is such an entry 
    let seller_data = (token, { owner = seller ; amt = amt ;}) in 
    let () = (
        match (Big_map.find_opt seller_data storage.tokens_for_sale) with
        | None -> (failwith error_NO_SUCH_ENTRY : unit)
        | Some u -> u
    ) in 

    // send the tokens to the buyer
    let txndata_send_tokens = 
        (Tezos.self_address, [ (buyer, token.token_id, amt) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" token.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_tokens = 
        Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 

    // update storage
    let updated_tokens_for_sale = Big_map.update seller_data (None : unit option) storage.tokens_for_sale in 

    // send the XTZ along to the seller (owner)
    let xtz_amt = Tezos.amount in 
    let entrypoint_pay_seller = (
        match (Tezos.get_contract_opt  seller : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some e -> e
    ) in 
    let op_pay_seller = Tezos.transaction () xtz_amt entrypoint_pay_seller in 

    [op_send_tokens ; op_pay_seller ;], {storage with tokens_for_sale = updated_tokens_for_sale ;}


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
    | PostForSale param -> 
        post_for_sale param storage
    | UnpostForSale param ->
        unpost_for_sale param storage
    | BuyForSale param ->
        buy_for_sale param storage
    | WhitelistTokens param -> 
        whitelist_tokens (param, storage)