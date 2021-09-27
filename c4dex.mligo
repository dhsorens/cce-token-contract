// the DEX that services the carbon contract 

(* =============================================================================
 * Storage
 * ============================================================================= *)

type token = { token_address : address ; token_id : nat ; }
type bal = nat 

type storage = {
    carbon_contract : address ;
    lp_token_contract : address ;
    token_whitelist : (token, unit) big_map ; 
}

(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type trade = { trade_from : token ; trade_for : token ; trade_amt : nat ; }
type whitelist_tokens = token * (unit option)
type add_liquidity = { input_token : token ; trade_amt : nat ; }
type remove_liquidity = { output_token : token ; trade_amt : nat ; }

type entrypoint = 
| Trade of trade 
| WhitelistTokens of whitelist_tokens
| AddLiquidity of add_liquidity
| RemoveLiquidity of remove_liquidity 

type result = operation list * storage

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

// a price function which maps dy -> dx, giving the price of y in terms of x
let trade_x_to_y (token_from : token) (token_to : token) (trade_amt : nat) : nat = 
    trade_amt

// given an input token it returns the corresponding LP tokens
let token_to_lp (token : token) (trade_amt : nat) : nat = 
    trade_amt

// given a desired output token and number of LP tokens traded it returns output token amt
let lp_to_token (token : token) (trade_amt : nat) : nat = 
    trade_amt

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// The sender has to make this contract an operator on their token before being able to make trades
let trade (param : trade) (storage : storage) : result = 
    let token_from = param.trade_from in 
    let token_to = param.trade_for in 
    let trade_amt = param.trade_amt in 

    // check that token_from and token_to are whitelisted tokens
    let () = (match (Big_map.find_opt token_from storage.token_whitelist) with | None -> (failwith "Token Not Whitelisted" : unit) | Some u -> u) in 
    let () = (match (Big_map.find_opt token_to   storage.token_whitelist) with | None -> (failwith "Token Not Whitelisted" : unit) | Some u -> u) in 

    // calculate the trade output
    let output_amt = trade_x_to_y token_from token_to trade_amt in 

    // execute the transfer into this contract 
    let txndata_receive_funds = 
        (Tezos.sender, [ (Tezos.self_address, token_from.token_id, trade_amt) ; ]) in
    let entrypoint_receive_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_from.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith "No Token Contract Found" : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_funds = 
        Tezos.transaction txndata_receive_funds 0tez entrypoint_receive_funds in 

    // execute the transfer out of this contract 
    let txndata_send_funds = 
        (Tezos.self_address, [ (Tezos.sender, token_to.token_id, output_amt) ; ]) in 
    let entrypoint_send_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_to.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith "No Token Contract Found" : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_funds = 
        Tezos.transaction txndata_send_funds 0tez entrypoint_send_funds in 
    
    // output
    ([op_receive_funds ; op_send_funds], storage)


let whitelist_tokens (param : whitelist_tokens) (storage : storage) : result = 
    if (Tezos.sender <> storage.carbon_contract) then (failwith "Permissions denied" : result) else 

    let (token, add_or_remove) = param in 
    let updated_whitelist : (token, unit) big_map = 
        Big_map.update token add_or_remove storage.token_whitelist in

    ([] : operation list), {storage with token_whitelist = updated_whitelist ;}


// trade input_token for LP tokens
let add_liquidity (param : add_liquidity) (storage : storage) : result = 
    let input_token = param.input_token in 
    let trade_amt = param.trade_amt in 

    // check the whitelist
    let () = (match (Big_map.find_opt input_token storage.token_whitelist) with | None -> (failwith "Token Not Whitelisted" : unit) | Some u -> u) in 

    let output_amt = token_to_lp input_token trade_amt in 

    // receive tokens
    let txndata_receive_tokens = 
        (Tezos.sender, [ (Tezos.self_address, input_token.token_id, trade_amt) ; ]) in 
    let entrypoint_receive_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" input_token.token_address : (address * (address * nat * nat) list) contract option) with
        | None -> (failwith "No Token Contract Found" : (address * (address * nat * nat) list) contract)
        | Some e -> e
    ) in 
    let op_receive_tokens = Tezos.transaction txndata_receive_tokens 0tez entrypoint_receive_tokens in 

    // mint LP tokens
    let txndata_mint_LP = [ (Tezos.sender, 0n, output_amt) ; ] in 
    let entrypoint_mint_LP = (
        match (Tezos.get_entrypoint_opt "%mint" storage.lp_token_contract : (address * nat * nat) list contract option) with
        | None -> (failwith "No LP Token Contract Found" : (address * nat * nat) list contract)
        | Some e -> e
    ) in 
    let op_mint_LP_tokens = Tezos.transaction txndata_mint_LP 0tez entrypoint_mint_LP in 

    [op_receive_tokens ; op_mint_LP_tokens], storage


let remove_liquidity (param : remove_liquidity) (storage : storage) : result = 
    let output_token = param.output_token in 
    let trade_amt = param.trade_amt in 

    // check the whitelist
    let () = (match (Big_map.find_opt output_token storage.token_whitelist) with | None -> (failwith "Token Not Whitelisted" : unit) | Some u -> u) in 

    // burn the LP tokens 
    let txndata_burn_LP = 
        [ (Tezos.sender, 0n, trade_amt) ; ] in 
    let entrypoint_burn_LP = (
        match (Tezos.get_entrypoint_opt "%burn" storage.lp_token_contract : (address * nat * nat) list contract option) with
        | None -> (failwith "No LP Token Contract Found" : (address * nat * nat) list contract)
        | Some e -> e
    ) in 
    let op_burn_LP_tokens = Tezos.transaction txndata_burn_LP 0tez entrypoint_burn_LP in 

    // send output_token
    let output_amt = lp_to_token output_token trade_amt in 
    let txndata_send_tokens = 
        (Tezos.self_address, [ (Tezos.sender, output_token.token_id, output_amt) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" output_token.token_address : (address * (address * nat * nat) list) contract option) with
        | None -> (failwith "No Token Contract Found" : (address * (address * nat * nat) list) contract)
        | Some e -> e
    ) in 
    let op_send_tokens = Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 

    [op_burn_LP_tokens ; op_send_tokens], storage


(* =============================================================================
 * Main
 * ============================================================================= *)

let main (entrypoint, storage : entrypoint * storage) = 
    match entrypoint with 
    | Trade param -> 
        trade param storage
    | WhitelistTokens param -> 
        whitelist_tokens param storage
    | AddLiquidity param ->
        add_liquidity param storage
    | RemoveLiquidity param -> 
        remove_liquidity param storage