(* Nth Fibonacci number *)

(*Recursion Based *)
let rec fib_rec n = match n with
    | 1 -> 1
    | 2 -> 1
    | _ -> (fib (n-1)) + (fib (n-2));;

let fib n = 
    let cache n = Array.make n 0 in
    let rec fib_dp n = match n with
        | 1 -> 1
        | 2 -> 1
        | _ -> let res1 = (if Array.(n-1)>0 then Array.(n-1) else fib_dp(n-1));
               Array.set cache n-1 res1;
               let res2 = (if Array.(n-2)>0 then Array.(n-2) else fib_dp(n-2));
               Array.set cache n-2 res2; in
    cache n;
    fib_dp n;;
    
    
    
    
let res n = if Array.(n)>0 then Array.(n) else fib_dp(n);
           Array.set cache n res1;;
           

let rec fib_dp n = match n with
    | 1 -> 1
    | 2 -> 1
    | _ -> let res1 = (if Array.(n-1)>0 then Array.(n-1) else fib_dp(n-1));
           Array.set cache n-1 res1;
           let res2 = (if Array.(n-2)>0 then Array.(n-2) else fib_dp(n-2));
           Array.set cache n-2 res2