#ifndef SOME_GUAD_ICE
#define SOME_GUAD_ICE

#include <SomeOtherModule.ice>
#include <YetAnoterOtherModule.ice>

module SomeModule
{
	exception SomeModuleException {};

	exception SomeModule1Exception extends SomeModuleException { string field1; };
	exception SomeModule2Exception extends SomeModuleException { string field_with_underscores_2; };
	exception SomeModule3Exception extends SomeModuleException { string fieldWithCamelCase3; };

	enum SomeModuleEnum
	{
		SomeModuleNew,
		SomeModuleAllGood,
		SomeModuleAllBad
	};

	sequence<long> LongSeq;

	struct SomeModuleResult
	{
		SomeModuleLevelStatus status;
		LongSeq scores;
	};

	struct OtherThingStatus
	{
		long internalOtId;
		OtherNs::OtherThingState otherThingState;
	};

	struct OtherThingResult
	{
		string otherThingId;
		SomeModuleResult firstSomeModuleResult;
		SomeModuleResult secondSomeModuleResult;
		OtherNs::OtherThingState otherThingState;
	};
	sequence<OtherThingResult> OtherThingResultSeq;

	struct AggregateOtherThingResult
	{
		long numFirstSomeModuleAllGood;
		long numFirstSomeModuleNew;
		long numFirstSomeModuleAllBad;
		long numSecondSomeModuleAllGood;
		long numSecondSomeModuleNew;
		long numSecondSomeModuleAllBad;
		long numOtherThingPending;
		long numOtherThingSucceeded;
		long numOtherThingFailed;
		long numOtherThingInDoubt;
	};

	interface HappinessEstimator
	{
		// The following two datatypes are chosen for laziness reasons right now -- something more appropriate might be in order 
		SomeModuleResult firstSomeModule(OtherNs::OtherSubNs::StartOtherThingRequest input);
		SomeModuleResult secondSomeModule(OtherNs::OtherSubNs::AdditionalOtherThingInputRequest input);
		// void setOtherThingStatus(OtherNs::OtherThingStatus status);

		OtherThingResultSeq getOtherThingResultsPerProvider(string providerId, long startTimestampIncl, long endTimestampExcl);
		AggregateOtherThingResult getAggregateOtherThingResultsPerProvider(string providerId, long startTimestampIncl, long endTimestampExcl);
	};

	struct HappinessRecipe
	{
		long id;
		string ruleName;
		long ruleWeight;
	};
	sequence<HappinessRecipe> HappinessRecipeSeq;

	struct HappinessRecipeSetInfo
	{
		long id;
		long nextRecipeLevel; // some comment
		long fraudLevel;		// some other comment
		/* yet another comment!! */    
		HappinessRecipeSeq rules;
	};

	interface HappinessRecipeSet
	{
		HappinessRecipeSetInfo getInfo();
		void addRecipe(HappinessRecipe rule);
		void updateRecipe(HappinessRecipe newRecipe);
		void removeRecipe(long ruleId);
		void setNextRecipeLevel(long val);
		void setAllBadLevel(long val);
		void remove();
	};
	sequence<HappinessRecipeSet*> HappinessRecipeSetSeq;

	struct HappinessThingInfo
	{
		string happinessThingId;
		HappinessRecipeSetSeq ruleSets;
	};

	interface HappinessThing
	{
		string getId();
		HappinessThingInfo getInfo();
		HappinessRecipeSet* addRecipeSet();
		// void removeRecipeSet(long id);
		HappinessRecipeSet* getRecipeSetById(long id);
		HappinessRecipeSetSeq getRecipeSets();
	};

	interface HappinessThingFactory
	{
		HappinessThing* getHappinessThing(string happinessThingId);
		HappinessThing* createHappinessThing(string happinessThingId) throws HappinessThingIdAlreadyExistsException, PersistStoreException;
		void removeHappinessThing(string happinessThingId) throws NoSuchHappinessThingIdException, PersistStoreException;
	};
};

#endif
