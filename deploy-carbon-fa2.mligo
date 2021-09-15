#include "carbon-fa2-types.mligo"
type storage_fa2 = storage

let deploy_carbon_fa2 (delegate : key_hash option) (amnt : tez) (init_storage : storage_fa2) = 
    Tezos.create_contract
    (fun (entrypoint, storage : entrypoint * storage) -> (
        let error_FA2_INSUFFICIENT_BALANCE = 1n in
        let error_FA2_NOT_OWNER = 3n in
        let error_FA2_NOT_OPERATOR = 4n in
        let error_PERMISSIONS_DENIED = 10n in
        let rec main ((entrypoint, storage) : entrypoint * storage) : result =
            (match entrypoint with
            | Transfer param -> (
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
                    main (Transfer(new_param), new_storage)
                | [] -> (([] : operation list), storage)
            )
            | Balance_of param -> (
                let (request_list, callback) = param in
                let accumulator = ([] : (fa2_owner * fa2_token_id * fa2_amt) list) in

                // define this function
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
                in

                let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.fa2_ledger) in
                let t = Tezos.transaction ack_list 0mutez callback in
                ([t], storage)
            )
                
            | Update_operators param -> (
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
            )
            | Mint param -> (
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
                    main (Mint(tl), storage)        
            )
            | Burn param -> (
                if Tezos.sender <> storage.carbon_contract then 
                    (failwith error_PERMISSIONS_DENIED : result) else 

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
                main (Transfer(txndata_burn), storage)
            )
            | Get_metadata _param -> (
                (([] : operation list), storage) // TODO : Metadata details TBD
            )) in
        main (entrypoint, storage)))
        (* End of contract code for the project FA2 contract *)
    delegate
    amnt 
    init_storage
    

