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
type transfer_to = [@layout:comb]{ to_ : address ; token_id : nat ; amount : nat ; }
type transfer = 
    [@layout:comb]
    { from_ : address; 
      txs : transfer_to list; }

type requests = [@layout:comb]{ owner : address ; token_id : nat ; }
type request = [@layout:comb]{ owner : address ; token_id : nat ; }
type callback_data = [@layout:comb]{ request : request ; balance : nat ; }
type balance_of = [@layout:comb]{
    requests : requests list ; 
    callback : callback_data list contract ;
}

type operator_data = [@layout:comb]{ owner : address ; operator : address ; token_id : nat ; }
type update_operator = 
    | Add_operator of operator_data
    | Remove_operator of operator_data
type update_operators = update_operator list

type mintburn_data = { owner : address ; token_id : nat ; qty : nat ; }
type mintburn = mintburn_data list

type token_data = { token_id : nat ; token_metadata : (string, bytes) map ; }
type get_metadata = {
    token_ids : nat list ;
    callback : token_data list contract ;
}

type entrypoint = 
| Transfer of transfer list // transfer tokens 
| Balance_of of balance_of // query an address's balance
| Update_operators of update_operators // change operators for some address
| Mint of mintburn // mint tokens
| Burn of mintburn // burn tokens 
| Get_metadata of get_metadata // query the metadata of a given token
| Add_zone of token_data list 