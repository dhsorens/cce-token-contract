(* ============================================================================
 * SRC: FA2 Carbon Contract 
 * ============================================================================ *)

#include "../carbon.mligo"
let  main_carbon = main
type storage_carbon = storage 
type entrypoint_carbon = entrypoint 
type result_carbon = result

#include "../carbon-fa2.mligo"
let  main_fa2 = main
type storage_fa2 = storage 
type entrypoint_fa2 = entrypoint 
type result_fa2 = result

#include "../carbon-amm.mligo"
let  main_amm = main
type storage_amm = storage 
type entrypoint_amm = entrypoint 
type result_amm = result

#include "../carbon-life.mligo"
let  main_life = main
type storage_life = storage 
type entrypoint_life = entrypoint 
type result_life = result


// TODO : AMM Tests 