deferred class
    AN_ABSTRACT_CLASS

feature

    a_deferred_feature
        -- a feature whose implementation is left to a descendent
        deferred
        end

    an_effective_feature: STRING
        -- deferred (abstract) classes may still include effective features
        do
            Result := "I am implemented!"
        end

end
