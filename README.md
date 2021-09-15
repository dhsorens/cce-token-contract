# FA2 Contract for the CCE Carbon Token

This repository is for the FA2 contract that represents the carbon token developed by the Centre for Carbon Economics at the University of Cambridge.

## General Structure

The main, "admin" contract is `carbon.mligo`. Through this contract you can:
1. Create a project 
1. Add a new token (zone) to your project 
1. Update permissions

The token contract represents a project. It has powers to mint tokens corresponding to the project's different zones. It is also a generic FA2 contract, so it has standard entrypoints such as `Transfer`, `Balance_of`, etc. This contract manages the project's tokens as well as some admin work that only the project owner has permissions to.

## Create a Project

As the code stands, to create a project, you provide the token ids and metadata corresponding to your different zones to the `carbon.mligo` contract. It will originate an FA2 contract for you with the different token types with corresponding metadata, and will 

TODO : 
The Carbon contract should grant permissions to let projects add zones (new token ids) or create new projects. This will be done via a whitelist, a datatype which keeps track of who has permissions. The company will update this whitelist when someone gets certified, and then that person will be able to query the Carbon contract to either add a token id (zone) to their existing project (FA2) contract or to originate a project.

As it stands, each address can only be associated with one contract.
<!-- 
## Add a New Token (Zone)

As the code stands, to add a new token, you provide the new token id and metadata to the Carbon contract. If you already have a project (existing FA2 contract), it will automatically add that token for you and you will have powers to mint.

TODO :
update permissions as described in the section above.
-->

## Mint

A project owner is the admin (operator) for all of their tokens and can unilaterally mint them. To do so they simply query their FA2 contract with the token id, the amount to mint, and the owner of these new tokens.

TODO : 
Total supply should be capped by the Carbon contract to keep the token holders from surprising the market and devaluing the tokens in circulation by minting more.


## FA2 Functionality

Users and token holders wishing to manage their tokens, i.e. with the `Transfer` or `Balance_of` functions, can query the project's FA2 contract and it functions as normal.


# Contract Structures 

There are currently two primary contracts in this project. These are:
1. `carbon.mligo` : the main, controller contract. This contract handles burning and project creation. <!-- will eventually handle add token -->
1. `carbon-fa2.mligo` : the contract that represents a projects and manages the project's different tokens, including NFTs.

The other two contracts, `carbon-fa2-types.mligo` and `deploy-carbon-fa2.mligo` are auxiliary to `carbon.mligo`, in particular in spawning up an FA2 contract as part of the `%createProject` entrypoint.

<!---
There are four primary contracts in this project. These are:
1. `carbon.mligo` : the main, controller contract 
1. `carbon-fa2.mligo` : the contract that manages a project, including a project's different tokens, including NFTs
1. `carbon-amm.mligo` : the contract that manages the carbon token market 
1. `carbon-life.mligo` : the contract that manages the LIFE token fa2 contract
--->