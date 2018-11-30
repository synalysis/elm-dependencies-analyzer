module StepResult exposing
    ( StepResult(..)
    , andThen
    , endErr
    , endOk
    , flippedAndThen
    , flippedAndThen2
    , fromResultContinue
    , map
    )

{-| Result of a step in a process which can either end with a Result, or needs to be continued.
-}


type StepResult error end continue
    = Continue continue
    | End (Result error end)



-- BUILD


endErr : error -> StepResult error x y
endErr error =
    End <| Err error


endOk : end -> StepResult x end y
endOk end =
    End <| Ok end


{-| Create StepResult from Result, mapping `Ok` to `Continue`
-}
fromResultContinue : Result error continue -> StepResult error x continue
fromResultContinue coreResult =
    case coreResult of
        Ok continue ->
            Continue continue

        Err error ->
            endErr error



-- MAPPING


map : (a -> value) -> StepResult x y a -> StepResult x y value
map fn result =
    case result of
        Continue value ->
            Continue <| fn value

        End end ->
            End end



-- CHAINING


andThen : (a -> StepResult x y b) -> StepResult x y a -> StepResult x y b
andThen fn result =
    case result of
        Continue value ->
            fn value

        End end ->
            End end


flippedAndThen :
    StepResult x y a
    -> (a -> StepResult x y b)
    -> StepResult x y b
flippedAndThen result fn =
    andThen fn result


flippedAndThen2 :
    StepResult x y a
    -> StepResult x y b
    -> (a -> b -> StepResult x y c)
    -> StepResult x y c
flippedAndThen2 resultA resultB fn =
    case resultA of
        Continue valueA ->
            case resultB of
                Continue valueB ->
                    fn valueA valueB

                End end ->
                    End end

        End end ->
            End end
