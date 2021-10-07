# FA2 Contract for the CCE Carbon Token

This repository is for the FA2 contract that represents the carbon token developed by the Cambridge Centre for Carbon Credits.

## Creating a Project

The main, "admin" contract is `carbon.mligo`. Through this contract you can:
1. Create a project 
<!--1. Add a new token (zone) to your project -->
1. Mint tokens for a project you own
1. "Bury" a carbon token that you own

When a user creates a project, they submit to the `carbon` contract a datatype containing distinct token ids (zones in the project) and metadata corresponding to each of the zones. The `carbon` contract will then deploy an FA2 contract specific to that project. The project owner (creator) can then mint tokens corresponding to captured carbon in each of their zones.

## Buying and Selling Carbon Tokens
Once a project owner has minted tokens, they can post them for sale by using the `c4x` contract. The `c4x` contract is a double-sided auction, which allows a token owner to:
1. Post tokens for sale
1. Auction off tokens 
1. Accept an offer made by a prospective buyer for their tokens

It also allows a prospective buyer to:
1. Buy tokens on sale
1. Participate in an auction 
1. Make an offer to a prospective seller for their carbon tokens

## FA2 Functionality

Projects are managed by an FA2 contract with all the standard entrypoints. Thus users and token holders can manage their tokens, i.e. with the `Transfer` or `Balance_of` entrypoints. This makes these carbon tokens composable with other on-chain applications.


# Contract Structures 

There are three primary contracts:
1. `carbon.mligo` : the main, controller contract, which manages permissions and allows for creating a project, minting tokens, and burning ("burying") tokens.
1. `carbon-fa2.mligo` : the template contract for a project, which manages the project's different tokens as a standard FA2 contract.
1. `c4x.mligo` : the double-sided auction contract which allows users to post tokens for sale in various ways, and for buyers to purchase tokens.
<!-- 
1. `bury.mligo` : the contract that manages the BURY token
-->

There are various auxiliary contracts in the `aux` folder.