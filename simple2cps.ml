let cps_program_of_simple_term : Simple.term -> Cps.program =
  fun tm -> (tm
  |> Simple2named.named_of_simple_term
  |> Named2sequential.sequential_of_named_term
  |> Sequential2cps.cps_program_of_sequential_term)
