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
type burn = (fa2_owner * fa2_token_id * fa2_amt) list
type get_metadata = fa2_token_id list


type entrypoint = 
| Transfer of transfer 
| Balance_of of balance_of
| Update_operators of update_operators
| Mint of mint
| Burn of burn
| Get_metadata of get_metadata