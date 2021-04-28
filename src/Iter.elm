-- SPDX-License-Identifier: MPL-2.0


module Iter exposing
    ( Iter, next, step
    , empty, count, countFrom, range, fromList, from
    , collect, fold
    , take, drop, map, filter, filterMap, groupsOf, zip
    )

{-| Iterators

@docs Iter, next, step
@docs empty, count, countFrom, range, fromList, from
@docs collect, fold
@docs take, drop, map, filter, filterMap, groupsOf, zip

-}


type Iter a s
    = Iter
        { state : s
        , next_ : Next a s
        }


type alias Next a s =
    s -> Maybe ( a, s )


next : Iter a s -> Maybe a
next (Iter { state, next_ }) =
    Maybe.map Tuple.first (next_ state)


step : Iter a s -> ( Maybe a, Iter a s )
step ((Iter { state, next_ }) as iter) =
    case next_ state of
        Just ( a, newState ) ->
            ( Just a, Iter { state = newState, next_ = next_ } )

        Nothing ->
            ( Nothing, iter )



-- Build iterators


empty : Iter a b
empty =
    Iter
        { state = Debug.todo "state"
        , next_ = \_ -> Nothing
        }


count : Iter Int Int
count =
    countFrom 0


countFrom : Int -> Iter Int Int
countFrom start =
    Iter
        { state = start
        , next_ = \n -> Just ( n, n + 1 )
        }


range : Int -> Int -> Iter Int Int
range start end =
    Iter
        { state = start
        , next_ =
            \n ->
                if n < end then
                    Just ( n, n + 1 )

                else
                    Nothing
        }


fromList : List a -> Iter a (List a)
fromList list =
    Iter
        { state = list
        , next_ = headAndTail
        }


headAndTail : List a -> Maybe ( a, List a )
headAndTail list =
    case list of
        x :: xs ->
            Just ( x, xs )

        [] ->
            Nothing


from : s -> Next a s -> Iter a s
from s next_ =
    Iter { state = s, next_ = next_ }



-- collect iterators


collect : Iter a s -> List a
collect (Iter { state, next_ }) =
    List.reverse (collectAcc state next_ [])


collectAcc : s -> Next a s -> List a -> List a
collectAcc state next_ acc =
    case next_ state of
        Just ( a, newState ) ->
            collectAcc newState next_ (a :: acc)

        Nothing ->
            acc


fold : (a -> b -> b) -> b -> Iter a s -> b
fold f acc (Iter { state, next_ }) =
    foldAcc state next_ f acc


foldAcc : s -> Next a s -> (a -> b -> b) -> b -> b
foldAcc state next_ f acc =
    case next_ state of
        Just ( a, newState ) ->
            foldAcc newState next_ f (f a acc)

        Nothing ->
            acc



-- Transform iterators


take : Int -> Iter a s -> Iter a ( Int, s )
take n (Iter { state, next_ }) =
    Iter
        { state = ( n, state )
        , next_ = takeNext next_
        }


takeNext : Next a s -> Next a ( Int, s )
takeNext next_ ( n, s ) =
    if n > 0 then
        case next_ s of
            Just ( a, newState ) ->
                Just ( a, ( n - 1, newState ) )

            Nothing ->
                Nothing

    else
        Nothing


drop : Int -> Iter a s -> Iter a s
drop n (Iter { state, next_ }) =
    dropAcc n state next_


dropAcc : Int -> s -> Next a s -> Iter a s
dropAcc n s next_ =
    if n <= 0 then
        Iter { state = s, next_ = next_ }

    else
        case next_ s of
            Just ( _, newState ) ->
                dropAcc (n - 1) newState next_

            Nothing ->
                Iter { state = s, next_ = next_ }


map : (a -> b) -> Iter a s -> Iter b s
map f (Iter { state, next_ }) =
    let
        mapNext s =
            case next_ s of
                Just ( a, newState ) ->
                    Just ( f a, newState )

                Nothing ->
                    Nothing
    in
    Iter
        { state = state
        , next_ = mapNext
        }


filter : (a -> Bool) -> Iter a s -> Iter a s
filter predicate (Iter { state, next_ }) =
    Iter { state = state, next_ = filterNext predicate next_ }


filterNext : (a -> Bool) -> Next a s -> Next a s
filterNext predicate next_ s =
    case next_ s of
        (Just ( a, newState )) as justNew ->
            if predicate a then
                justNew

            else
                filterNext predicate next_ newState

        Nothing ->
            Nothing


filterMap : (a -> Maybe b) -> Iter a s -> Iter b s
filterMap maybePredicate (Iter { state, next_ }) =
    Iter { state = state, next_ = filterMapNext maybePredicate next_ }


filterMapNext : (a -> Maybe b) -> Next a s -> Next b s
filterMapNext maybePredicate next_ s =
    case next_ s of
        Just ( a, newState ) ->
            case maybePredicate a of
                Just b ->
                    Just ( b, newState )

                Nothing ->
                    filterMapNext maybePredicate next_ newState

        Nothing ->
            Nothing


groupsOf : Int -> Iter a s -> Iter (List a) s
groupsOf size (Iter { state, next_ }) =
    Iter { state = state, next_ = nextGroupOf size 1 [] next_ }


nextGroupOf : Int -> Int -> List a -> Next a s -> Next (List a) s
nextGroupOf size accSize acc next_ s =
    case next_ s of
        Just ( a, newState ) ->
            if accSize == size then
                Just ( List.reverse (a :: acc), newState )

            else
                nextGroupOf size (accSize + 1) (a :: acc) next_ newState

        Nothing ->
            if accSize > 1 then
                Just ( List.reverse acc, s )

            else
                Nothing


zip : Iter a1 s1 -> Iter a2 s2 -> Iter ( a1, a2 ) ( s1, s2 )
zip (Iter iter1) (Iter iter2) =
    let
        zipNext ( s1, s2 ) =
            case ( iter1.next_ s1, iter2.next_ s2 ) of
                ( Just ( a1, newS1 ), Just ( a2, newS2 ) ) ->
                    Just ( ( a1, a2 ), ( newS1, newS2 ) )

                _ ->
                    Nothing
    in
    Iter
        { state = ( iter1.state, iter2.state )
        , next_ = zipNext
        }
