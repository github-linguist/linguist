
/*
	Converts a string to the corresponding list element from a particular list. Note the element doesn't need to be in the list variable at that moment in time! 

	Useful for sending parameters into the ink from the game: the game can store and pass in the string ID of the list element as a parameter.

	Returns the empty list () if the element isn't found.

	Usage: 

	LIST capitalCities = Paris, London, NewYork

	~ temp thisCity = string_to_list("Paris", capitalCities)
	~ capitalCities += thisCity
	I've now visited {thisCity}.

	Optimisation:

	The code below works in inky, but can be externalised to speed up performance in game, with the following external C# function binding:

	story.BindExternalFunction("STRING_TO_LIST", (string itemKey) => {
        try
        {
            return InkList.FromString(itemKey, story);
        }
        catch
        {
            return new InkList();
        }
    }, true);

*/

=== function string_to_list(stringElement, listSource)
    ~ temp retVal = STRING_TO_LIST(stringElement) 
    { USED_STRING_TO_LIST_FALLBACK:
    	~ retVal = stringAsPickedFromList(stringElement, LIST_ALL(listSource) ) 
    }
     ~ return retVal


EXTERNAL STRING_TO_LIST(stringElement) 
=== function STRING_TO_LIST(stringElement) 
    ~ return USED_STRING_TO_LIST_FALLBACK()

=== function USED_STRING_TO_LIST_FALLBACK() 
	// this stub function is used to detect that the game isn't using an external function
    ~ return     

// fallback system: recurse through the listToTry, trying to string match the element name
=== function stringAsPickedFromList(stringElement, listToTry)
    ~ temp minElement = LIST_MIN(listToTry) 
    {minElement:
        { stringElement == "{minElement}":
            ~ return minElement
        }
        ~ return stringAsPickedFromList(stringElement, listToTry - minElement)
    }       
    ~ return () 