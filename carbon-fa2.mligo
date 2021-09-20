(* An FA2 contract for carbon tokens *)
(* FA2 Proposal TZIP: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md *)
(* FA2 Standard: https://tezos.b9lab.com/fa2 *)


(* =============================================================================
 * Storage
 * ============================================================================= *)

type fa2_from = address
type fa2_to = address 
type fa2_token_id = nat
type fa2_amt = nat
type fa2_owner = address
type fa2_operator = address

type token_metadata = (string, bytes) map

type storage = {
    // address of the main carbon contract
    carbon_contract : address ; 

    // the ledger keeps track of who owns what token
    ledger : (fa2_owner * fa2_token_id , fa2_amt) big_map ; 
    
    // an operator can trade tokens on behalf of the fa2_owner
    // if the key (owner, operator, token_id) returns () this denotes that the operator has permissions
    // if there is no entry, the operator has no permissions
    // such permissions need to granted, e.g. for the burn entrypoint in the carbon contract
    operators : (fa2_owner * fa2_operator * fa2_token_id, unit) big_map; 
    
    // token metadata for each token type supported by this contract
    metadata : (fa2_token_id, token_metadata) big_map; 
}
// TODO: keep track of owner in storage?

type result = (operation list) * storage


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type transfer = fa2_from * ((fa2_to * fa2_token_id * fa2_amt) list)
type balance_of = ((fa2_owner * fa2_token_id) list) * ((fa2_owner * fa2_token_id * fa2_amt) list contract)
type update_operators = 
    | Add_operator of fa2_owner * fa2_operator * fa2_token_id
    | Remove_operator of fa2_owner * fa2_operator * fa2_token_id
type mint = (fa2_owner * fa2_token_id * fa2_amt) list
type burn = (fa2_owner * fa2_token_id * fa2_amt) list
type get_metadata = fa2_token_id list * ((fa2_token_id * token_metadata) list contract)


type entrypoint = 
| Transfer of transfer // transfer tokens 
| Balance_of of balance_of // query an address's balance
| Update_operators of update_operators // change operators for some address
| Mint of mint // mint tokens
| Burn of burn // burn tokens 
| Get_metadata of get_metadata // query the metadata of a given token


(* =============================================================================
 * Error codes
 * ============================================================================= *)

let error_FA2_TOKEN_UNDEFINED = 0n // One of the specified token_ids is not defined within the FA2 contract
let error_FA2_INSUFFICIENT_BALANCE = 1n // A token owner does not have sufficient balance to transfer tokens from owner's account
let error_FA2_TX_DENIED = 2n // A transfer failed because of fa2_operatortransfer_policy == No_transfer
let error_FA2_NOT_OWNER = 3n // A transfer failed because fa2_operatortransfer_policy == fa2_ownertransfer and it is invoked not by the token owner
let error_FA2_NOT_OPERATOR = 4n // A transfer failed because fa2_operatortransfer_policy == fa2_owneror_fa2_operatortransfer and it is invoked neither by the token owner nor a permitted operator
let error_FA2_OPERATORS_UNSUPPORTED = 5n // update_operators entrypoint is invoked and fa2_operatortransfer_policy is No_transfer or fa2_ownertransfer
let error_FA2_RECEIVER_HOOK_FAILED = 6n // The receiver hook failed. This error MUST be raised by the hook implementation
let error_FA2_SENDER_HOOK_FAILED = 7n // The sender failed. This error MUST be raised by the hook implementation
let error_FA2_RECEIVER_HOOK_UNDEFINED = 8n // Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
let error_FA2_SENDER_HOOK_UNDEFINED = 9n // Sender hook is required by the permission behavior, but is not implemented by a sender contract
let error_PERMISSIONS_DENIED = 10n // General catch-all for operator-related permission errors
let error_ID_ALREADY_IN_USE = 11n // A token ID can only be used once, error if a user wants to add a token ID that's already there

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

// an auxiliary function for querying an address's balance
let rec owner_and_id_to_balance (param : ((fa2_owner * fa2_token_id * fa2_amt) list) * ((fa2_owner * fa2_token_id) list) * ((fa2_owner * fa2_token_id , fa2_amt) big_map)) : (fa2_owner * fa2_token_id * fa2_amt) list =
    let (accumulator, request_list, ledger) = param in
    match request_list with
    | [] -> accumulator 
    | h :: t -> 
        let (owner, token_id) = h in 
        let amt =
            (match (Big_map.find_opt (owner, token_id) ledger) with 
            | None -> 0n
            | Some owner_balance -> owner_balance)
        in
        let accumulator = (owner, token_id, amt) :: accumulator in
        let updated_request_list = t in
        owner_and_id_to_balance (accumulator, updated_request_list, ledger) 


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// The transfer entrypoint function
// The input type is a tuple: (sender, list_of_transfers) where the first entry corresponds 
//    to the sender ("from"), and the second is a list with transfer data.
// This list of transfers has entries of the form (receiver, token_id, amount) = (address * nat * nat)
// The transfer function creates a list of transfer operations recursively
let rec transfer (param , storage : transfer * storage) : result = 
    let (fa2_from, transfers_list) = param in
    match transfers_list with
    | hd :: tl ->
        let (fa2_to, fa2_token_id, fa2_amt) = hd in
        // check permissions: is Tezos.sender an operator or owner of this token?
        let fa2_operator = Tezos.sender in 
        let fa2_owner = fa2_from in 
        let not_operator : bool = 
            (match Big_map.find_opt (fa2_owner, fa2_operator, fa2_token_id) storage.operators with 
            | None -> true
            | Some () -> false ) in 
        if ((Tezos.sender <> fa2_from) && not_operator) then (failwith error_FA2_NOT_OPERATOR : result) else 

        let big_map_key = (fa2_from, fa2_token_id) in
        let ledger = storage.ledger in
        let sender_token_balance =
            (match (Big_map.find_opt big_map_key ledger) with
            | None -> 0n
            | Some token_balance -> token_balance)
        in
        if (sender_token_balance < fa2_amt) then (failwith error_FA2_INSUFFICIENT_BALANCE : result) else
        let new_ledger_0 = (Big_map.update (fa2_from, fa2_token_id) (Some (abs (sender_token_balance - fa2_amt))) storage.ledger) in 
        let recipient_balance = 
            (match (Big_map.find_opt (fa2_to, fa2_token_id) ledger) with
            | None -> 0n
            | Some recipient_token_balance -> recipient_token_balance)
        in
        let new_ledger = (Big_map.update (fa2_to, fa2_token_id) (Some (recipient_balance + fa2_amt)) new_ledger_0) in 
        let new_storage = {storage with ledger = new_ledger} in
        let new_param = (fa2_from, tl) in
        transfer (new_param, new_storage)
    | [] -> (([] : operation list), storage)


// the entrypoint to query balance 
// input balance_of is a tuple:
//   * the first entry is a list of the form (owner, token_id) list which queries the balance of owner in the given token id
//   * the second entry is a contract that can receive the list of balances. This list is of the form 
//     (owner, token_id, amount) list = (address * nat * nat) list
//     An example of such a contract is in tests/test-fa2.mligo 
let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = param in
    let accumulator = ([] : (fa2_owner * fa2_token_id * fa2_amt) list) in
    let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.ledger) in
    let t = Tezos.transaction ack_list 0mutez callback in
    ([t], storage)


// The entrypoint where fa2_owner adds or removes fa2_operator from storage.operators
// * The input is a triple: (owner, operator, id) : address * address * nat
//   This triple is tagged either as Add_operator or Remove_operator
// * Only the token owner can add or remove operators
// * An operator can perform transactions on behalf of the owner
let update_operators (param : update_operators) (storage : storage) : result = 
    match param with
    | Add_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        if (Tezos.source <> fa2_owner) then (failwith error_PERMISSIONS_DENIED : result) else
        let new_operators = Big_map.update (fa2_owner, fa2_operator, fa2_token_id) (Some ()) storage.operators in 
        let storage = {storage with operators = new_operators} in 
        (([] : operation list), storage)
    | Remove_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        if (Tezos.sender <> fa2_owner) then (failwith error_PERMISSIONS_DENIED : result) else
        let new_operators = Big_map.update (fa2_owner,fa2_operator,fa2_token_id) (None : unit option) storage.operators in
        let storage = {storage with operators = new_operators} in 
        (([] : operation list), storage)


// only the carbon contract can mint tokens
// This entrypoint can only be called by the carbon contract
let rec mint_tokens (param, storage : mint * storage) : result =
    let minting_list = param in
    match minting_list with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let (fa2_owner, fa2_token_id, fa2_amt) = hd in
        if Tezos.sender <> storage.carbon_contract then (failwith error_FA2_NOT_OPERATOR : result) else 
        let fa2_ownerbalance = 
            (match (Big_map.find_opt (fa2_owner, fa2_token_id) storage.ledger) with
            | None -> 0n
            | Some fa2_ownerprev_balance -> fa2_ownerprev_balance)
        in
        let new_fa2_ownerbalance = fa2_ownerbalance + fa2_amt in
        let new_ledger = Big_map.update (fa2_owner, fa2_token_id) (Some new_fa2_ownerbalance) storage.ledger in
        let storage = {storage with ledger = new_ledger} in 
        mint_tokens (tl, storage)

// only the carbon contract can burn tokens
// Like minting, this entrypoint can only be called by the carbon contract
let burn_tokens (param : burn) (storage : storage) : transfer = 
    if Tezos.sender <> storage.carbon_contract then (failwith error_PERMISSIONS_DENIED : transfer) else 
    let burn_addr = ("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" : address) in 
    let addr_from = Tezos.source in 

    // transfer the tokens to the burn address
    let txndata_burn : transfer = 
        (
            addr_from,
            List.map
            (fun (owner,id,amt : address * nat * nat) -> 
                let () = assert (owner = addr_from) in 
                (burn_addr, id, amt) )
            param
        )
    in 
    txndata_burn

// The entrypoint to query token metadata
// The input is a tuple: (query_list, callback_contract)
//   * The query list is of token ids and has type `nat list`
//   * The callback contract must have type ((fa2_token_id * token_metadata) list contract)
let get_metadata (param : get_metadata) (storage : storage) : result = 
    let (query_list, callback) = param in 
    let metadata_list = 
        List.map 
        (fun (id : nat) -> 
            match Big_map.find_opt id storage.metadata with 
            | None -> (failwith error_FA2_TOKEN_UNDEFINED : nat * token_metadata) 
            | Some m -> (id, m))
        query_list in 

    let op_metadata = Tezos.transaction metadata_list 0tez callback in 
    ( [op_metadata] , storage)

(* =============================================================================
 * Main
 * ============================================================================= *)

let rec main ((entrypoint, storage) : entrypoint * storage) : result =
    match entrypoint with
    | Transfer param ->
        transfer (param, storage)
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators param storage
    | Mint param -> 
        mint_tokens (param, storage)
    | Burn param ->
        main (Transfer( burn_tokens param storage ), storage)
    | Get_metadata param ->
        get_metadata param storage
