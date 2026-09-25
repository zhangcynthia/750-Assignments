---
title: Subtask 4 Description
output: html_document
---

## Optimization: Predicate Pushdown
`optimize_pipeline` (in `R/optimize.R`) implements predicate pushdown. The function takes a `Pipeline` as input and moves `keep` or `remove` steps as early as possible in the steps list, without changing the final result.

The optimization works as: For a given `keep` or `remove` step, I write a `get_condition_vars` function to extract the column the step depends on from the condition. Then use a `can_move` function to determine if it is safe to move this filter step one step forward. 

The determinations of whether safe to move forward or not are :

- select: safe only if all columns the filter depends on are among the selected columns.
- exclude: safe only if none of the filter's columns are excluded.
- derive: safe only if the derived column's name does not collide with a column the filter depends on.
- map: safe only if the mapped column is not one the filter depends on.
- keep/remove: always safe (filtering operations commute).
- Other operation (`rename`, `rearrange`, `reorder`, `group_by_aggregate`): treated as unsafe, so filters are never moved past them.

`optimize_pipeline` repeatedly scans the steps list and move the filter step forward if `can_move` function allows it, looping until no move is allowed.

`describe_pipeline` is a helper function that print the sequence of operation names in a pipeline. It can be used to compare the operation orderings before and after optimization.


## Testing
Tests are in `tests/testthat/test-pipeline.R`. One test confirms a keep step is pushed as far forward as safely possible. Another test confirms that a keep step is not moved across a select that would drop the column it depends on. A third test confirms that optimization does not change the final result: executing a pipeline before and after optimization produces the same columns and column names.

All tests pass using `devtools::test()`.
