(* This is the DEX that services the carbon contract *)

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

type token_to_token = { trade_from : token ; trade_for : token ; trade_amt : nat ; }
type token_to_tez = { token_from : token ; trade_amt : nat ; }
type tez_to_token = { token_to   : token ; }
type whitelist_tokens = (token * (unit option)) list
type add_liquidity = { input_token : token ; trade_amt : nat ; }
type remove_liquidity = { output_token : token ; trade_amt : nat ; }

type entrypoint = 
| TokenToToken of token_to_token 
| TokenToTez of token_to_tez
| TezToToken of tez_to_token
| WhitelistTokens of whitelist_tokens
| AddLiquidity of add_liquidity
| RemoveLiquidity of remove_liquidity 

type result = operation list * storage

(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_NO_TOKEN_CONTRACT_FOUND = 0n
let error_TOKEN_NOT_WHITELISTED = 1n
let error_INVALID_ADDRESS = 2n
let error_PERMISSIONS_DENIED = 3n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

// a price function which maps dy -> dx, giving the price of y in terms of x
let trade_x_to_y (token_from : token) (token_to : token) (trade_amt : nat) : nat = 
    trade_amt

let trade_tez_to_y (token_to : token) (trade_amt : nat) : nat = 
    trade_amt

let trade_x_to_tez (token_from : token) (trade_amt : nat) : nat = 
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

// trade XTZ for tokens or tokens for XTZ
let tez_to_token (param : tez_to_token) (storage : storage) : result = 
    let trade_amt = Tezos.amount / 1mutez in 
    let token_to = param.token_to in 

    // contract has received XTZ, now must send the desired coin
    let () = (match (Big_map.find_opt token_to storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 

    let output_amt = trade_tez_to_y token_to trade_amt in 

    // execute the transfer to send funds
    let txndata_send_funds = 
        (Tezos.self_address, [ (Tezos.sender, token_to.token_id, output_amt) ; ]) in 
    let entrypoint_send_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_to.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_funds = 
        Tezos.transaction txndata_send_funds 0tez entrypoint_send_funds in 

    ([op_send_funds], storage)

let token_to_tez (param : token_to_tez) (storage : storage) : result = 
    let trade_amt = param.trade_amt in 
    let token_from = param.token_from in 

    // receive funds 
    let txndata_receive_funds = 
        (Tezos.sender, [ (Tezos.self_address, token_from.token_id, trade_amt) ; ]) in
    let entrypoint_receive_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_from.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_funds = 
        Tezos.transaction txndata_receive_funds 0tez entrypoint_receive_funds in 

    // send XTZ to Tezos.sender
    let output_amt = 1mutez * (trade_x_to_tez token_from trade_amt) in 
    let entrypoint_send_tez = (
        match (Tezos.get_contract_opt Tezos.sender : unit contract option) with 
        | None -> (failwith error_INVALID_ADDRESS : unit contract)
        | Some e -> e
    ) in 
    let op_send_tez = Tezos.transaction () output_amt entrypoint_send_tez in

    [ op_receive_funds ; op_send_tez ; ], storage


// The sender has to make this contract an operator on their token before being able to make trades
let token_to_token (param : token_to_token) (storage : storage) : result = 
    let token_from = param.trade_from in 
    let token_to = param.trade_for in 
    let trade_amt = param.trade_amt in 

    // check that token_from and token_to are whitelisted tokens
    let () = (match (Big_map.find_opt token_from storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 
    let () = (match (Big_map.find_opt token_to   storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 

    // calculate the trade output
    let output_amt = trade_x_to_y token_from token_to trade_amt in 

    // execute the transfer into this contract 
    let txndata_receive_funds = 
        (Tezos.sender, [ (Tezos.self_address, token_from.token_id, trade_amt) ; ]) in
    let entrypoint_receive_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_from.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in
    let op_receive_funds = 
        Tezos.transaction txndata_receive_funds 0tez entrypoint_receive_funds in 

    // execute the transfer out of this contract 
    let txndata_send_funds = 
        (Tezos.self_address, [ (Tezos.sender, token_to.token_id, output_amt) ; ]) in 
    let entrypoint_send_funds = (
        match (Tezos.get_entrypoint_opt "%transfer" token_to.token_address : (address * (address * nat * nat) list) contract option) with 
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e ) in 
    let op_send_funds = 
        Tezos.transaction txndata_send_funds 0tez entrypoint_send_funds in 
    
    // output
    ([op_receive_funds ; op_send_funds], storage)


let rec whitelist_tokens (param, storage : whitelist_tokens * storage) : result = 
    if (Tezos.sender <> storage.carbon_contract) then (failwith error_PERMISSIONS_DENIED : result) else 

    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl ->
        let (token, add_or_remove) = hd in 
        let updated_whitelist : (token, unit) big_map = 
            Big_map.update token add_or_remove storage.token_whitelist in

        whitelist_tokens (tl, {storage with token_whitelist = updated_whitelist ;})


// trade input_token for LP tokens
let add_liquidity (param : add_liquidity) (storage : storage) : result = 
    let input_token = param.input_token in 
    let trade_amt = param.trade_amt in 

    // check the whitelist
    let () = (match (Big_map.find_opt input_token storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 

    let output_amt = token_to_lp input_token trade_amt in 

    // receive tokens
    let txndata_receive_tokens = 
        (Tezos.sender, [ (Tezos.self_address, input_token.token_id, trade_amt) ; ]) in 
    let entrypoint_receive_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" input_token.token_address : (address * (address * nat * nat) list) contract option) with
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e
    ) in 
    let op_receive_tokens = Tezos.transaction txndata_receive_tokens 0tez entrypoint_receive_tokens in 

    // mint LP tokens
    let txndata_mint_LP = [ (Tezos.sender, 0n, output_amt) ; ] in 
    let entrypoint_mint_LP = (
        match (Tezos.get_entrypoint_opt "%mint" storage.lp_token_contract : (address * nat * nat) list contract option) with
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * nat * nat) list contract)
        | Some e -> e
    ) in 
    let op_mint_LP_tokens = Tezos.transaction txndata_mint_LP 0tez entrypoint_mint_LP in 

    [op_receive_tokens ; op_mint_LP_tokens], storage


let remove_liquidity (param : remove_liquidity) (storage : storage) : result = 
    let output_token = param.output_token in 
    let trade_amt = param.trade_amt in 

    // check the whitelist
    let () = (match (Big_map.find_opt output_token storage.token_whitelist) with | None -> (failwith error_TOKEN_NOT_WHITELISTED : unit) | Some u -> u) in 

    // burn the LP tokens 
    let txndata_burn_LP = 
        [ (Tezos.sender, 0n, trade_amt) ; ] in 
    let entrypoint_burn_LP = (
        match (Tezos.get_entrypoint_opt "%burn" storage.lp_token_contract : (address * nat * nat) list contract option) with
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * nat * nat) list contract)
        | Some e -> e
    ) in 
    let op_burn_LP_tokens = Tezos.transaction txndata_burn_LP 0tez entrypoint_burn_LP in 

    // send output_token
    let output_amt = lp_to_token output_token trade_amt in 
    let txndata_send_tokens = 
        (Tezos.self_address, [ (Tezos.sender, output_token.token_id, output_amt) ; ]) in 
    let entrypoint_send_tokens = (
        match (Tezos.get_entrypoint_opt "%transfer" output_token.token_address : (address * (address * nat * nat) list) contract option) with
        | None -> (failwith error_NO_TOKEN_CONTRACT_FOUND : (address * (address * nat * nat) list) contract)
        | Some e -> e
    ) in 
    let op_send_tokens = Tezos.transaction txndata_send_tokens 0tez entrypoint_send_tokens in 

    [op_burn_LP_tokens ; op_send_tokens], storage


(* =============================================================================
 * Main
 * ============================================================================= *)

let main (entrypoint, storage : entrypoint * storage) = 
    match entrypoint with 
    | TokenToToken param -> 
        token_to_token param storage
    | TokenToTez param ->
        token_to_tez param storage 
    | TezToToken param ->
        tez_to_token param storage
    | AddLiquidity param ->
        add_liquidity param storage
    | RemoveLiquidity param -> 
        remove_liquidity param storage
    | WhitelistTokens param -> 
        whitelist_tokens (param, storage)