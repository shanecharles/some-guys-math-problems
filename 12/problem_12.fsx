let divisors (x : int) = 
  let sqrt = float >> System.Math.Sqrt >> int
  seq { for i in {1 .. sqrt x} do
          match (x % i, x / i) with 
          | 0, y when i <> y -> yield i; yield y
          | 0, _             -> yield i
          | _                -> () } 

let triangularNumbers = Seq.unfold (fun (i,acc) -> Some (acc + i, (i + 1, acc + i))) (1, 0)

triangularNumbers
|> Seq.filter (fun x -> x |> divisors |> Seq.length > 500) 
|> Seq.head
|> printfn "The value of frist triangular number with over 500 divisors: %d"