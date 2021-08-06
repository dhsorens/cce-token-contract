(* An FA2 contract for carbon tokens *)
(* FA2 Proposal TZIP: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md *)
(* FA2 Standard: https://tezos.b9lab.com/fa2 *)


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
    fa2_ledger : (owner_ , (token_id_ * amt_)) big_map ;
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

// "FA2_TOKEN_UNDEFINED" : One of the specified token_ids is not defined within the FA2 contract
// "FA2_INSUFFICIENT_BALANCE" : A token owner does not have sufficient balance to transfer tokens from owner's account
// "FA2_TX_DENIED" : A transfer failed because of operator_transfer_policy == No_transfer
// "FA2_NOT_OWNER" : A transfer failed because operator_transfer_policy == Owner_transfer and it is invoked not by the token owner
// "FA2_NOT_OPERATOR" : A transfer failed because operator_transfer_policy == Owner_or_operator_transfer and it is invoked neither by the token owner nor a permitted operator
// "FA2_OPERATORS_UNSUPPORTED" : update_operators entrypoint is invoked and operator_transfer_policy is No_transfer or Owner_transfer
// "FA2_RECEIVER_HOOK_FAILED" : The receiver hook failed. This error MUST be raised by the hook implementation
// "FA2_SENDER_HOOK_FAILED" : The sender failed. This error MUST be raised by the hook implementation
// "FA2_RECEIVER_HOOK_UNDEFINED" : Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
// "FA2_SENDER_HOOK_UNDEFINED" : Sender hook is required by the permission behavior, but is not implemented by a sender contract

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)



(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let transfer (param : transfer) (storage : storage) : result = 
    // if Tezos.sender <> owner then failwith
    // if balance insufficient then failwith
    // if no balance/entry in the big_map then failwith
    // otherwise update the ledger
    (([] : operation list), storage)

let balance_of (param : balance_of) (storage : storage) : result = 
    // check permissions
    // return amt_
    (([] : operation list), storage)

let update_operators (param : update_operators) (storage : storage) : result = 
    // check permissions
    // match param with Add_operator or Remove_operator
    // update the big_map
    (([] : operation list), storage)

let mint (param : mint) (storage : storage) : result = 
    // if Tezos.sender <> an_operator then failwith
    // Otherwise mint by adding balance
    (([] : operation list), storage)

let burn (param : burn) (storage : storage) : result = 
    // if Tezos.sender <> owner then failwith
    // Otherwise burn by subtracting balance
    (([] : operation list), storage)


(* =============================================================================
 * Main
 * ============================================================================= *)

let main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with
    | Transfer param ->
        transfer param storage
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators param storage
    | Mint param -> 
        mint param storage
    | Burn param ->
        burn param storage