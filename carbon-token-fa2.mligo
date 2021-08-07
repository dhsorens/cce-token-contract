(* An FA2 contract for carbon tokens *)
(* FA2 Proposal TZIP: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md *)
(* FA2 Standard: https://tezos.b9lab.com/fa2 *)


// TODO : 
// TODO : Total supply
// TODO : Permission rights


(* =============================================================================
 * Storage
 * ============================================================================= *)

type from_ = address
type to_ = address 
type token_id_ = nat
type amt_ = nat
type owner_ = address
type operator_ = address


type storage = {
    fa2_ledger : (owner_ * token_id_ , amt_) big_map ;
    operators : (operator_, token_id_) big_map;
}


type result = (operation list) * storage


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type transfer = from_ * ((to_ * token_id_ * amt_) list)
type balance_of = ((owner_ * token_id_) list) * ((owner_ * token_id_ * amt_) list contract)
type update_operators = 
    | Add_operator of owner_ * operator_ * token_id_
    | Remove_operator of owner_ * operator_ * token_id_
type mint = (owner_ * amt_) list
type burn = (owner_ * amt_) list

type entrypoint = 
| Transfer of transfer 
| Balance_of of balance_of
| Update_operators of update_operators
| Mint of mint
| Burn of burn


(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_FA2_TOKEN_UNDEFINED = 0n // One of the specified token_ids is not defined within the FA2 contract
let error_FA2_INSUFFICIENT_BALANCE = 1n // A token owner does not have sufficient balance to transfer tokens from owner's account
let error_FA2_TX_DENIED = 2n // A transfer failed because of operator_transfer_policy == No_transfer
let error_FA2_NOT_OWNER = 3n // A transfer failed because operator_transfer_policy == Owner_transfer and it is invoked not by the token owner
let error_FA2_NOT_OPERATOR = 4n // A transfer failed because operator_transfer_policy == Owner_or_operator_transfer and it is invoked neither by the token owner nor a permitted operator
let error_FA2_OPERATORS_UNSUPPORTED = 5n // update_operators entrypoint is invoked and operator_transfer_policy is No_transfer or Owner_transfer
let error_FA2_RECEIVER_HOOK_FAILED = 6n // The receiver hook failed. This error MUST be raised by the hook implementation
let error_FA2_SENDER_HOOK_FAILED = 7n // The sender failed. This error MUST be raised by the hook implementation
let error_FA2_RECEIVER_HOOK_UNDEFINED = 8n // Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
let error_FA2_SENDER_HOOK_UNDEFINED = 9n // Sender hook is required by the permission behavior, but is not implemented by a sender contract

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)



(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let rec transfer (param , storage : transfer * storage) : result = 
    let (from_, transfers_list) = param in // param : from_ * ((to_ * token_id_ * amt_) list)
    if Tezos.sender <> from_ then (failwith error_FA2_NOT_OWNER : result) else 
    match transfers_list with
    | hd :: tl ->
        let (to_, token_id_, amt_) = hd in
        let big_map_key = (from_, token_id_) in
        let fa2_ledger = storage.fa2_ledger in
        match (Big_map.find_opt big_map_key fa2_ledger) with
        | None -> (failwith error_FA2_INSUFFICIENT_BALANCE : result)
        | Some token_balance -> 
            if (balance < amt) then (failwith error_FA2_INSUFFICIENT_BALANCE : result) else
            let new_fa2_ledger_1 = (Big_map.update (from, token_id) (Some (balance - amt)) storage.fa2_ledger) in 
            let recipient_balance = 
                match (Big_map.find_opt (to_, token_id) fa2_ledger) with
                | None -> 0n
                | Some recipient_token_balance -> recipient_token_balance
            in
            let new_fa2_ledger = (Big_map.update (to_, token_id) (Some recipient_balance + amt)) in 
            let new_storage = {storage with fa2_ledger = new_fa2_ledger} in
            let new_param = (from_, tl) in
            transfer (new_param, new_storage)
        //transfer ((from_, tl), storage)
    | [] -> (([] : operation list), storage)

let balance_of (param : balance_of) (storage : storage) : result = 
    // check permissions
    // return amt_
    let ( l1 , l2 ) = param in // param : ((owner_ * token_id_) list) * ((owner_ * token_id_ * amt_) list contract)
    (([] : operation list), storage)

let update_operators (param : update_operators) (storage : storage) : result = 
    // check permissions
    // match param with Add_operator or Remove_operator
    // update the big_map
    match param with
    | Add_operator (owner_, operator_, token_id_) -> (([] : operation list), storage)
    | Remove_operator (owner_, operator_, token_id_) -> (([] : operation list), storage)
    

let mint (param : mint) (storage : storage) : result = 
    // if Tezos.sender <> an_operator then failwith
    // Otherwise mint by adding balance
    let minting_list = param in // param : (owner_ * amt_) list
    (([] : operation list), storage)

let burn (param : burn) (storage : storage) : result = 
    // if Tezos.sender <> owner then failwith
    // Otherwise burn by subtracting balance
    let burning_list = param in // param : (owner_ * amt_) list
    (([] : operation list), storage)


(* =============================================================================
 * Main
 * ============================================================================= *)

let main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with
    | Transfer param ->
        transfer (param, storage)
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators param storage
    | Mint param -> 
        mint param storage
    | Burn param ->
        burn param storage