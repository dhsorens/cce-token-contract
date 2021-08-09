(* An FA2 contract for carbon tokens *)
(* FA2 Proposal TZIP: https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md *)
(* FA2 Standard: https://tezos.b9lab.com/fa2 *)


// TODO : 
// TODO : Total supply
// TODO : Permission rights, Tezos.source vs Tezos.sender


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

let rec owner_and_id_to_balance (accumulator : (owner_ * token_id_ * amt_) list) (request_list : (owner_ * token_id_) list) (fa2_ledger : (owner_ * token_id_ , amt_) big_map) : (owner_ * token_id_ * amt_) list =
    match request_list with
    | [] -> accumulator 
    | request :: requests ->
        let (owner, token_id) = request in 
        let amt = 
        match (Big_map.find_opt request fa2_ledger) with
        | None -> 0n
        | Some owner_balance -> owner_balance
        in 
        let accumulator = (owner, token_id, amt) :: accumulator 
        in owner_and_id_to_balance accumulator 


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
    let (request_list, callback) = param in // param : ((owner_ * token_id_) list) * ((owner_ * token_id_ * amt_) list contract)
    let ([] : (owner_ * token_id_ * amt_) list)) = accumulator in
    let ack_list = owner_and_id_to_balance accumulator request_list storage.fa2_ledger in
    let t = Tezos.transaction ack_list 0n callback in
    ([t], storage)

let update_operators (param : update_operators) (storage : storage) : result = 
    match param with
    | Add_operator (owner_, operator_, token_id_) -> 
        match (Big_map.find_opt owner_ storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some result_token_id -> 
            if result_token_id <> token_id_ then (failwith error_FA2_NOT_OPERATOR : result) else
            let new_operators = Big_map.update operator_ (Some token_id_) storage.operators in
            let storage = {storage with operators = new_operators} in 
            (([] : operation list), storage)
    | Remove_operator (owner_, operator_, token_id_) ->
        match (Big_map.find_opt owner_ storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some result_token_id -> 
            if result_token_id <> token_id_ then (failwith error_FA2_NOT_OPERATOR : result) else
            match (Big_map.find_opt operator_ token_id_) with
            | None -> (([] : operation list), storage) // Nothing happens
            | Some operator_token_id ->
                if operator_token_id <> token_id_ then (([] : operation list), storage) else
                let new_operators = Big_map.update operator_ None storage.operators in
                let storage = {storage with operators = new_operators} in 
                (([] : operation list), storage)
    

let rec mint (param : mint) (storage : storage) : result = 
    let minting_list = param in // param : (owner_ * token_id_ * amt_) list
    match minting_list with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let (owner_, token_id_, amt_) = hd in
        match (Big_map.find_opt (Tezos.sender, token_id_) storage.fa2_ledger) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some sender_token_id_privelege -> 
            if sender_token_id_privelege <> token_id then (failwith error_FA2_NOT_OPERATOR : result) else
            let owner_balance = 
                match (Big_map.find_opt (owner_, token_id_) storage.fa2_ledger) with
                | None -> 0n
                | Some owner_prev_balance -> owner_prev_balance
            let new_owner_balance = owner_balance + amt_ in
            let new_fa2_ledger = Big_map.update (owner_, token_id_) (Some new_owner_balance) storage.fa2_ledger in
            let storage = {storage with fa2_ledger = new_fa2_ledger} in 
            mint tl storage

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