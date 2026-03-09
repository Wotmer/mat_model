-- Варианты ходов
-- silent это 0
-- testifies это 1
inductive Move where
  | silent
  | testifies
  deriving BEq

def opposite (move : Move) : Move :=
  match move with
  | .silent => .testifies
  | .testifies => .silent

-- Это для возможности вывода действий текстом
instance : ToString Move where
  toString move := match move with
  | .silent => "сотрудничать(0)"
  | .testifies => "предать(1)"

-- Очки для первого
def score (move₁ move₂ : Move) : Nat :=
  match move₁, move₂ with
  | .silent, .silent => 3
  | .silent, .testifies => 0
  | .testifies, .silent => 5
  | .testifies, .testifies => 1

-- Структура для стратегии
structure Strategy (Q : Type) where
  initState : Q
  action : Q → Move
  nextState : Q → Move → Q

-- всегда предаёт (1)
def Alex : Strategy Unit where
  initState := ()
  action _ := .testifies
  nextState _ _ := ()

-- всегда сотрудничает (0)
def Bob : Strategy Unit where
  initState := ()
  action _ := .silent
  nextState _ _ := ()

-- в начале 0, затем копирует
def Clara : Strategy Move where
  initState := .silent
  action q := q
  nextState _ opponentMove := opponentMove

-- в начале 0, затем противоположное
def Denis : Strategy Move where
  initState := .silent
  action q := q
  nextState _ opponentMove := opposite opponentMove

-- всегда 0, но 1 раз в 20 ходов выдаёт 1
def Emma : Strategy Nat where
  initState := 1
  action q := if q == 0 then .testifies else .silent
  nextState q _ := (q + 1) % 20

-- всегда 0, пока не предали
def Frida : Strategy Bool where
  initState := true
  action q := if q then .silent else .testifies
  nextState q opponentMove :=
    if !q then false
    else opponentMove == .silent

-- в начале 0, потом копирует, но вместо каждого 10-ого предательства сотрудничает
def George : Strategy (Move × Nat) where
  initState := (.silent, 0)
  action q :=
    if q.snd = 0 then .silent
    else q.fst
  nextState q opponentMove :=
    let count :=
      if opponentMove == .testifies then
        (q.snd + 1) % 10
      else
        q.snd
    (opponentMove, count)

-- Это для того, чтобы метрики считать
structure GameStats where
  scoreA : Nat := 0
  scoreB : Nat := 0
  streakA_curr : Nat := 0
  streakA_max : Nat := 0
  streakB_curr : Nat := 0
  streakB_max : Nat := 0

def playOneGame {Q₁ Q₂ : Type} (strategy₁ : Strategy Q₁) (strategy₂ : Strategy Q₂)
    (nRounds : Nat) : Nat × Nat :=
  go strategy₁ strategy₂ nRounds strategy₁.initState strategy₂.initState
where
  go {Q₁ Q₂ : Type} (strategy₁ : Strategy Q₁) (strategy₂ : Strategy Q₂)
    (nRounds : Nat) (q1 : Q₁) (q2 : Q₂) :=
    match nRounds with
    | 0 => (0, 0)
    | nRounds + 1 =>
      let move1 := strategy₁.action q1
      let move2 := strategy₂.action q2
      -- dbg_trace f!"{move1} {move2}"
      let res1 := score move1 move2
      let res2 := score move2 move1
      let next := go strategy₁ strategy₂ nRounds (strategy₁.nextState q1 move2) (strategy₂.nextState q2 move1)
      (res1 + next.fst, res2 + next.snd)

#eval playOneGame George Clara 20

def play {Q₁ Q₂ : Type} (strategy₁ : Strategy Q₁) (strategy₂ : Strategy Q₂)
    (nRounds : Nat) : GameStats :=
  go strategy₁ strategy₂ nRounds strategy₁.initState strategy₂.initState {}
where
  go {Q₁ Q₂ : Type} (strategy₁ : Strategy Q₁) (strategy₂ : Strategy Q₂)
  (nRounds : Nat) (q₁ : Q₁) (q₂ : Q₂) (stats : GameStats) : GameStats :=
  match nRounds with
  | 0 => stats
  | n + 1 =>
    let m₁ := strategy₁.action q₁
    let m₂ := strategy₂.action q₂
    let s₁ := score m₁ m₂
    let s₂ := score m₂ m₁
    let currA := if s₁ == 5 then stats.streakA_curr + 1 else 0
    let currB := if s₂ == 5 then stats.streakB_curr + 1 else 0

    let newStats : GameStats := {
      scoreA := stats.scoreA + s₁
      scoreB := stats.scoreB + s₂
      streakA_curr := currA
      streakA_max := max stats.streakA_max currA
      streakB_curr := currB
      streakB_max := max stats.streakB_max currB
    }
    go strategy₁ strategy₂ n (strategy₁.nextState q₁ m₂) (strategy₂.nextState q₂ m₁) newStats



inductive AnyStrategy where
  | mk {Q : Type} (name : String) (strat : Strategy Q)

def allStrategies : List AnyStrategy := [
  .mk "Alex" Alex,
  .mk "Bob" Bob,
  .mk "Clara" Clara,
  .mk "Denis" Denis,
  .mk "Emma" Emma,
  .mk "Frida" Frida,
  .mk "George" George
]

def runMatch (s₁ s₂ : AnyStrategy) (nRounds : Nat) : GameStats :=
  match s₁, s₂ with
  | .mk _ strat₁, .mk _ strat₂ => play strat₁ strat₂ nRounds

def padString (s : String) (len : Nat) : String :=
  let p := if len > s.length then len - s.length else 0
  s ++ (String.ofList (List.replicate p ' '))

def printRow (name : String) (cols : List String) : IO Unit := do
  let mut res := padString name 10
  for c in cols do
    res := res ++ "| " ++ padString c 8
  IO.println res

def main : IO Unit := do
  let strats := allStrategies
  let names := strats.map (fun s => match s with | .mk n _ => n)
  let rounds := 200

  IO.println "Общее число очков"
  printRow "" names
  for s1 in strats do
    let s1Name := match s1 with | .mk n _ => n
    let mut cols := []
    for s2 in strats do
      let stats := runMatch s1 s2 rounds
      cols := cols ++ [toString stats.scoreA]
    printRow s1Name cols

  IO.println "\nНаибольшая доминирующая серия"
  printRow "" names
  for s1 in strats do
    let s1Name := match s1 with | .mk n _ => n
    let mut cols := []
    for s2 in strats do
      let stats := runMatch s1 s2 rounds
      cols := cols ++ [toString stats.streakA_max]
    printRow s1Name cols

  IO.println "\nИтоги"
  printRow "Стратегия" ["Очки"]
  for s1 in strats do
    let s1Name := match s1 with | .mk n _ => n
    let mut totalScore := 0
    for s2 in strats do
      let stats := runMatch s1 s2 rounds
      totalScore := totalScore + stats.scoreA
    printRow s1Name [toString totalScore]

#eval main
