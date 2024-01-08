open System.IO

module Types =

    type Suit =
        | Diamonds
        | Hearts
        | Clubs
        | Spades

    type Face =
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King
        | Ace

    type Card = { Face: Face; Suit: Suit; Value: uint }

    type Hand = Hand of Card list

open Types

module RankParser =
    type PairSum = PairSum of uint
    type ThreeOfAKindSum = ThreeOfAKindSum of uint
    type FourOfAKindSum = FourOfAKindSum of uint
    type ConsecutiveValuesSum = ConsecutiveValuesSum of uint
    type SuitSum = SuitSum of uint

    type Rank =
        | HighCard of uint
        | OnePair of PairSum
        | TwoPairs of PairSum * PairSum
        | ThreeOfAKind of ThreeOfAKindSum
        | Straight of ConsecutiveValuesSum
        | Flush of SuitSum
        | FullHouse of ThreeOfAKindSum * PairSum
        | FourOfAKind of FourOfAKindSum
        | StraightFlush of ConsecutiveValuesSum
        | RoyalFlush of SuitSum

    type ParsingHit =
        { Rank: Rank
          Hand: Hand
          Remaining: Card List }

    type RankParsingResult<'a> =
        | Miss of Hand
        | Hit of 'a

    type RankParser = Hand -> RankParsingResult<ParsingHit>

    let bind f result =
        match result with
        | Miss h -> f h
        | Hit h -> Hit h

    let (>=>) p f = p >> (bind f)

    let combine f result =
        match result with
        | Miss h -> Miss h
        | Hit firstHit ->
            match f (Hand firstHit.Remaining) with
            | Miss h -> Miss h
            | Hit secondHit -> Hit(firstHit, secondHit)

    let (>>=) p f = p >> (combine f)

    let mapHit f result =
        match result with
        | Miss h -> Miss h
        | Hit h -> Hit(f h)

    let mapMiss f result =
        match result with
        | Miss h -> Miss(f h)
        | Hit h -> Hit h

    let nOf keySelector length ctor hand =
        let (Hand cards) = hand

        let pairs =
            cards
            |> List.groupBy keySelector
            |> List.sortByDescending (fun (k, v) -> v.Length)

        let remaining = pairs |> List.skip 1 |> List.collect snd

        match pairs with
        | (key, value) :: _ when value.Length = length ->
            Hit
                { Rank = (ctor key value)
                  Hand = hand
                  Remaining = remaining }
        | _ -> Miss hand

    let getPair rank =
        match rank with
        | OnePair p -> p
        | _ -> failwith "Rank does not contain desired choice"

    let getThreeOfAKind rank =
        match rank with
        | ThreeOfAKind f -> f
        | _ -> failwith "Rank does not contain desired choice"

    let getSuit rank =
        match rank with
        | Flush s -> s
        | _ -> failwith "Rank does not contain desired choice"

    let getConsecutiveValues rank =
        match rank with
        | Straight v -> v
        | _ -> failwith "Rank does not contain desired choice"

    let parseHighCard (hand: Hand) =
        let (Hand cards) = hand
        let highestCard = cards |> List.maxBy _.Value
        let remaining = cards |> List.filter (fun x -> x <> highestCard)

        Hit
            { Rank = HighCard highestCard.Value
              Hand = hand
              Remaining = remaining }

    let parseOnePair (hand: Hand) =
        nOf (_.Value) 2 (fun k v -> OnePair(PairSum(v |> List.sumBy _.Value))) hand

    let parseTwoPairs (hand: Hand) =
        hand
        |> (parseOnePair >>= parseOnePair
            >> mapHit (fun (first, second) ->
                { second with
                    Rank = TwoPairs(getPair first.Rank, getPair second.Rank) })
            >> mapMiss (fun _ -> hand))

    let parseThreeOfAKind (hand: Hand) =
        nOf (_.Value) 3 (fun k v -> v |> List.sumBy _.Value |> (ThreeOfAKindSum >> ThreeOfAKind)) hand

    let parseStraight (hand: Hand) =
        let (Hand cards) = hand
        let minValue = cards |> List.map _.Value |> List.min

        let allCardsAreConsecutiveValues =
            cards
            |> List.sortBy _.Value
            |> List.indexed
            |> List.forall (fun (i, c) -> c.Value = (uint i + minValue))

        if allCardsAreConsecutiveValues then
            Hit
                { Rank = Straight(ConsecutiveValuesSum(cards |> List.sumBy _.Value))
                  Hand = hand
                  Remaining = cards }
        else
            Miss hand

    let parseFlush (hand: Hand) =
        let (Hand cards) = hand

        match cards |> List.groupBy _.Suit with
        | [ (_, values) ] ->
            Hit
                { Rank = Flush(SuitSum(values |> List.sumBy _.Value))
                  Hand = hand
                  Remaining = cards }
        | _ -> Miss hand

    let parseFullHouse (hand: Hand) =
        hand
        |> (parseThreeOfAKind >>= parseOnePair
            >> mapHit (fun (first, second) ->
                { second with
                    Rank = FullHouse(getThreeOfAKind first.Rank, getPair second.Rank) })
            >> mapMiss (fun _ -> hand))

    let parseFourOfAKind (hand: Hand) =
        nOf (_.Value) 4 (fun k v -> v |> List.sumBy _.Value |> (FourOfAKindSum >> FourOfAKind)) hand

    let parseStraightFlush (hand: Hand) =
        hand
        |> (parseStraight >>= parseFlush
            >> mapHit (fun (first, second) ->
                { second with
                    Rank = StraightFlush(getConsecutiveValues first.Rank) })
            >> mapMiss (fun _ -> hand))

    let parseRoyalFlush (hand: Hand) =
        let (Hand cards) = hand

        let sumOfValues = cards |> List.map _.Value |> List.sum

        match parseFlush hand with
        | Hit h when sumOfValues = 60u ->
            Hit
                { h with
                    Rank = RoyalFlush(getSuit h.Rank) }
        | _ -> Miss hand

    let parseRank c =
        let parse: RankParser =
            parseRoyalFlush
            >=> parseStraightFlush
            >=> parseFourOfAKind
            >=> parseFullHouse
            >=> parseFlush
            >=> parseStraight
            >=> parseThreeOfAKind
            >=> parseTwoPairs
            >=> parseOnePair
            >=> parseHighCard

        match parse c with
        | Hit x -> x.Rank
        | Miss _ -> failwith "This will not gonna happen"

let parseCard (c: string) =
    let (face, value) =
        match c[0] with
        | '2' -> Two, 2
        | '3' -> Three, 3
        | '4' -> Four, 4
        | '5' -> Five, 5
        | '6' -> Six, 6
        | '7' -> Seven, 7
        | '8' -> Eight, 8
        | '9' -> Nine, 9
        | 'T' -> Ten, 10
        | 'J' -> Jack, 11
        | 'Q' -> Queen, 12
        | 'K' -> King, 13
        | 'A' -> Ace, 14
        | v -> failwith $"%c{v} is not supported"

    let suit =
        match c[1] with
        | 'C' -> Clubs
        | 'H' -> Hearts
        | 'D' -> Diamonds
        | 'S' -> Spades
        | v -> failwith $"%c{v} is not supported"

    { Face = face
      Suit = suit
      Value = uint value }

let parseHand hand =
    let cards = hand |> Seq.map parseCard |> Seq.toList

    Hand cards

let filePath = Path.Combine(__SOURCE_DIRECTORY__, "0054_poker.txt")

let hands =
    File.ReadAllLines filePath
    |> Seq.map _.Split(" ")
    |> Seq.map (fun x -> x |> Array.splitInto 2)
    |> Seq.map (fun x -> parseHand x[0], parseHand x[1])
    |> Seq.toList

open RankParser

let score (rank: RankParser.Rank) =
    match rank with
    | HighCard c -> c
    | OnePair(PairSum s) -> s
    | TwoPairs(PairSum f, PairSum s) -> f + s
    | ThreeOfAKind(ThreeOfAKindSum s) -> s
    | Straight(ConsecutiveValuesSum s) -> s
    | Flush(SuitSum s) -> s
    | FullHouse(ThreeOfAKindSum t, PairSum p) -> t + p
    | FourOfAKind(FourOfAKindSum s) -> s
    | StraightFlush(ConsecutiveValuesSum s) -> s
    | RoyalFlush(SuitSum s) -> s

type Player =
    | One
    | Two

let rec rankByHighestCards left right =
    let (Hand leftCards) = left
    let (Hand rightCards) = right

    if List.isEmpty leftCards || List.isEmpty rightCards then
        failwith "empty hands"

    let leftCardsSorted = leftCards |> List.sortByDescending _.Value
    let highestLeft = leftCardsSorted[0]

    let rightCardsSorted = rightCards |> List.sortByDescending _.Value
    let highestRight = rightCardsSorted[0]

    if highestLeft = highestRight then
        rankByHighestCards (Hand leftCardsSorted[1..]) (Hand rightCardsSorted[1..])
    elif highestLeft > highestRight then
        Player.One
    else
        Player.Two

let play (left, right) =

    let lRank = RankParser.parseRank left
    let rRank = RankParser.parseRank right

    match lRank, rRank with
    | l, r when l > r -> Player.One
    | l, r when l < r -> Player.Two
    | l, r ->
        let leftScore = score l
        let rightScore = score r

        if leftScore = rightScore then rankByHighestCards left right
        elif leftScore > rightScore then Player.One
        else Player.Two

let result =
    hands |> List.map play |> List.filter (fun x -> x = Player.One) |> List.length
