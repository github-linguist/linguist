declare
  %% Creates a new class derived from BaseClass
  %% with an added feature (==public immutable attribute)
  fun {AddFeature BaseClass FeatureName FeatureValue}
     class DerivedClass from BaseClass
        feat
	   %% "FeatureName" is escaped, otherwise a new variable
	   %% refering to a private feature would be created
           !FeatureName:FeatureValue
     end
  in
     DerivedClass
  end

  class Base
     feat
        bar:1

     meth init
        skip
     end
  end

  Derived = {AddFeature Base foo 2}

  Instance = {New Derived init}
in
  {Show Instance.bar} %% inherited feature
  {Show Instance.foo} %% feature of "synthesized" class
