data Nat = Zero | Succ Nat | Pred Nat

neg :: Nat -> Nat
neg Zero = Zero
neg (Succ n) = Pred (neg n)
neg (Pred n) = Succ (neg n)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)
add (Pred x) y = Pred (add x y)

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ x) y = add y (mult x y)
mult (Pred x) y = add (neg y) (mult x y)

sub :: Nat -> Nat -> Nat
sub x y = add x (neg y)