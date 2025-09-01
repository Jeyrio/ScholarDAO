# ScholarDAO

A decentralized scholarship fund built on Stacks where donors contribute STX and the community votes to allocate funding to students transparently.

## Features

- **Contribute**: Donors can contribute STX to the scholarship fund
- **Submit Proposals**: Students can submit scholarship proposals with funding requests
- **Community Voting**: Token holders vote on which proposals to fund
- **Transparent Execution**: Approved proposals are automatically funded via smart contract

## Contract Functions

### Public Functions
- `contribute(amount)` - Contribute STX to the scholarship fund
- `submit-proposal(amount, description)` - Submit a scholarship proposal
- `vote(proposal-id, support)` - Vote on a proposal (true for support, false against)
- `execute-proposal(proposal-id)` - Execute an approved proposal

### Read-Only Functions
- `get-proposal(proposal-id)` - Get proposal details
- `get-donor-contribution(donor)` - Get total contribution by a donor
- `get-total-fund()` - Get current fund balance

## Usage

1. Deploy the contract
2. Donors contribute using `contribute`
3. Students submit proposals using `submit-proposal`
4. Community votes using `vote`
5. Execute approved proposals using `execute-proposal`