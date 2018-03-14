# Shortest-Path
An interactive CLI for calculating the shortest path
This project uses stack:  https://github.com/commercialhaskell/stack

To run: `stack build && stack exec shortest-path-exe`

**Example usage:**
Create graph: 
`create {"a": {"b": 4, "c": 1}, "c": {"b": 1}}`

Update graph: 
`update {"a": {"c": 2}}`

Find shortest point:
`calculate a->b`

To see current graph:
`state`



