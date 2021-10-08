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
type transfer_to = [@layout:comb]{ to : address ; token_id : nat ; qty : nat ; }
type transfer = [@layout:comb]{
    from : address ; 
    to : transfer_to list ;
}

type owner_data = [@layout:comb]{ owner : address ; token_id : nat ; }
type token_ownership = [@layout:comb]{ owner : address ; token_id : nat ; qty : nat ; }
type balance_of = [@layout:comb]{
    owner_data : owner_data list ; 
    callback : token_ownership list contract ;
}

type operator_data = [@layout:comb]{ token_owner : address ; operator : address ; token_id : nat ; }
type update_operators = 
    | Add_operator of operator_data
    | Remove_operator of operator_data

type mintburn = token_ownership list

type callback_metadata = { token_id : nat ; token_metadata : (string, bytes) map ; }
type get_metadata = {
    token_ids : nat list ;
    callback : callback_metadata list contract ;
}

type entrypoint = 
| Transfer of transfer // transfer tokens 
| Balance_of of balance_of // query an address's balance
| Update_operators of update_operators // change operators for some address
| Mint of mintburn // mint tokens
| Burn of mintburn // burn tokens 
| Get_metadata of get_metadata // query the metadata of a given token
