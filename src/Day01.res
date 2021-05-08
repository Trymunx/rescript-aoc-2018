// parseInt returns an integeter parsed from a string, or 0 if not possible.
let parseInt = x => Belt_Int.fromString(x)->Belt_Option.getWithDefault(0)

let input =
  "input/01.txt"
  ->Node_fs.readFileSync(#utf8)
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Js.Array2.map(parseInt)

let part1 = input->Js.Array2.reduce((sum, n) => sum + n, 0)
Js.log2("Day 1 part 1", part1)

let rec solve = (input: array<int>, initial: int, seen: Belt_SetInt.t) =>
  switch Js.Array2.shift(input) {
  | Some(n) => {
      let sum = initial + n
      if Belt_SetInt.has(seen, sum) {
        sum
      } else {
        solve(Js.Array2.concat(input, [n]), sum, Belt_SetInt.add(seen, sum))
      }
    }
  | None => initial
  }

let part2 = input->solve(0, Belt_SetInt.fromArray([0]))
Js.log2("Day 1 part 2", part2)

// Tests
type test = {input: string, output: int}
let examples: array<test> = [
  {input: "+1, -1", output: 0},
  {input: "+3, +3, +4, -2, -4", output: 10},
  {input: "-6, +3, +8, +5, -6", output: 5},
  {input: "+7, +7, -2, -7, -4", output: 14},
]
Js.Array2.forEach(examples, example => {
  let output =
    example.input
    ->Js.String2.split(",")
    ->Js.Array2.map(parseInt)
    ->solve(0, Belt_SetInt.fromArray([0]))
  if output !== example.output {
    Js.log4("expected", example.output, "got", output)
  }
})
