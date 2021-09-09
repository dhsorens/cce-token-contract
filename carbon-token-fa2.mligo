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
    carbon_contract : address ; // address of the main carbon contract
    fa2_ledger : (fa2_owner * fa2_token_id , fa2_amt) big_map ;
    operators : (fa2_operator * fa2_token_id, unit) big_map;
    metadata : (fa2_token_id, token_metadata) big_map;
}
// TODO: Record type to name owner?

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
type burn = (fa2_owner * fa2_amt) list
type get_metadata = fa2_token_id list
type add_token = nat * token_metadata // token_id, metadata


type entrypoint = 
| Transfer of transfer 
| Balance_of of balance_of
| Update_operators of update_operators
| Mint of mint
| Burn of burn
| Get_metadata of get_metadata
| Add_token of add_token 


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
let error_PERMISSIONS_DENIED = 10n
let error_ID_ALREADY_IN_USE = 11n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let rec owner_and_id_to_balance (param : ((fa2_owner * fa2_token_id * fa2_amt) list) * ((fa2_owner * fa2_token_id) list) * ((fa2_owner * fa2_token_id , fa2_amt) big_map)) : (fa2_owner * fa2_token_id * fa2_amt) list =
    let (accumulator, request_list, fa2_ledger) = param in
    match request_list with
    | [] -> accumulator 
    | h :: t -> 
        let (owner, token_id) = h in 
        let amt =
            (match (Big_map.find_opt (owner, token_id) fa2_ledger) with 
            | None -> 0n
            | Some owner_balance -> owner_balance)
        in
        let accumulator = (owner, token_id, amt) :: accumulator in
        let updated_request_list = t in
        owner_and_id_to_balance (accumulator, updated_request_list, fa2_ledger) 


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let rec transfer (param , storage : transfer * storage) : result = 
    let (fa2_from, transfers_list) = param in
    if Tezos.source <> fa2_from then (failwith error_FA2_NOT_OWNER : result) else 
    match transfers_list with
    | hd :: tl ->
        let (fa2_to, fa2_token_id, fa2_amt) = hd in
        let big_map_key = (fa2_from, fa2_token_id) in
        let fa2_ledger = storage.fa2_ledger in
        let sender_token_balance =
            (match (Big_map.find_opt big_map_key fa2_ledger) with
            | None -> 0n
            | Some token_balance -> token_balance)
        in
        if (sender_token_balance < fa2_amt) then (failwith error_FA2_INSUFFICIENT_BALANCE : result) else
        let new_fa2_ledger_0 = (Big_map.update (fa2_from, fa2_token_id) (Some (abs (sender_token_balance - fa2_amt))) storage.fa2_ledger) in 
        let recipient_balance = 
            (match (Big_map.find_opt (fa2_to, fa2_token_id) fa2_ledger) with
            | None -> 0n
            | Some recipient_token_balance -> recipient_token_balance)
        in
        let new_fa2_ledger = (Big_map.update (fa2_to, fa2_token_id) (Some (recipient_balance + fa2_amt)) new_fa2_ledger_0) in 
        let new_storage = {storage with fa2_ledger = new_fa2_ledger} in
        let new_param = (fa2_from, tl) in
        transfer (new_param, new_storage)
    | [] -> (([] : operation list), storage)


let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = param in
    let accumulator = ([] : (fa2_owner * fa2_token_id * fa2_amt) list) in
    let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.fa2_ledger) in
    let t = Tezos.transaction ack_list 0mutez callback in
    ([t], storage)


// fa2_owner adds or removes fa2_operator from storage.operators
let update_operators (param : update_operators) (storage : storage) : result = 
    match param with
    | Add_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        (match (Big_map.find_opt (fa2_owner,fa2_token_id) storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some () ->
            let new_operators = Big_map.update (fa2_operator, fa2_token_id) (Some ()) storage.operators in
            let storage = {storage with operators = new_operators} in 
            (([] : operation list), storage)
        )
    | Remove_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        (match (Big_map.find_opt (fa2_owner,fa2_token_id) storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some () -> 
            (match (Big_map.find_opt (fa2_operator,fa2_token_id) storage.operators) with
            | None -> (([] : operation list), storage) // Nothing happens
            | Some () ->
                let new_operators = Big_map.update (fa2_operator,fa2_token_id) (None : unit option) storage.operators in
                let storage = {storage with operators = new_operators} in 
                (([] : operation list), storage)
            )
        )


let rec mint_tokens (param, storage : mint * storage) : result =
    let minting_list = param in
    match minting_list with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let (fa2_owner, fa2_token_id, fa2_amt) = hd in
        let txn_sender = Tezos.sender in
        let _has_privelege : unit = 
            (match (Big_map.find_opt (txn_sender,fa2_token_id) storage.operators) with
            | None -> (failwith error_FA2_NOT_OPERATOR : unit)
            | Some () -> () ) in 
        let fa2_ownerbalance = 
            (match (Big_map.find_opt (fa2_owner, fa2_token_id) storage.fa2_ledger) with
            | None -> 0n
            | Some fa2_ownerprev_balance -> fa2_ownerprev_balance)
        in
        let new_fa2_ownerbalance = fa2_ownerbalance + fa2_amt in
        let new_fa2_ledger = Big_map.update (fa2_owner, fa2_token_id) (Some new_fa2_ownerbalance) storage.fa2_ledger in
        let storage = {storage with fa2_ledger = new_fa2_ledger} in 
        mint_tokens (tl, storage)

let burn_tokens (_param : burn) (storage : storage) : result = (([] : operation list), storage) // TODO : Permissions TBD 

let get_metadata (_param : get_metadata) (storage : storage) : result = (([] : operation list), storage) // TODO : Metadata details TBD

let add_token (param : add_token) (storage : storage) : result = 
    if (Tezos.sender <> storage.carbon_contract) then 
        (failwith error_PERMISSIONS_DENIED : result) else 
    // if the above passes, then Tezos.source is the project owner 
    let owner = Tezos.source in 
    let (id,meta) = param in 
    
    // update operators in storage
    let new_operators = 
        Big_map.update (owner, id) (Some ()) storage.operators in 
    
    // update metadata in storage
    let _check_id_not_used = (
        match Big_map.find_opt id storage.metadata with 
        | None -> () 
        | Some _ -> (failwith error_ID_ALREADY_IN_USE : unit)
    ) in 
    let new_metadata = 
        Big_map.update id (Some meta) storage.metadata in 
    
    let updated_storage = { storage with 
        operators = new_operators ;
        metadata = new_metadata ;
    } in 

    (([] : operation list), updated_storage)

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
        mint_tokens (param, storage)
    | Burn param ->
        burn_tokens param storage
    | Get_metadata param ->
        get_metadata param storage
    | Add_token param ->
        add_token param storage
