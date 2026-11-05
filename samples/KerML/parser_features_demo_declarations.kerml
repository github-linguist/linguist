/*
 * Parser Features Demo: Declarations
 * 
 * Demonstrates advanced declaration patterns:
 * - Multiplicity declarations (Task 73)
 * - Subclassifier keyword (Task 74)
 * - Classifier keyword (Task 66)
 * - Multiple typing with comma (Task 78)
 * - Subset/disjoint statements (Task 65)
 * 
 * Based on real stdlib patterns from Base.kerml, Occurrences.kerml
 */

package Declarations {
    
    // The library is not implicitly visible: a bare name needs an import
    // (KerML 8.2.3.5 — only the top-level elements of a root namespace are).
    private import ScalarValues::*;
    private import Occurrences::Occurrence;
    
    // ===== Multiplicity Declarations (Task 73) =====
    
    multiplicity exactlyOne [1..1] {
        doc
        /* Exactly one element required */
    }
    
    multiplicity zeroOrOne [0..1] {
        doc
        /* Optional element */
    }
    
    multiplicity oneToMany [1..*];
    
    // ===== Classifier Keyword (Task 66) =====
    
    // 'classifier' is canonical KerML term
    abstract classifier Anything {
        doc
        /* Base classifier */
        
        feature self: Anything[1] redefines self;
    }
    
    classifier Thing specializes Anything {
        feature id: String;
    }
    
    // ===== Subclassifier Keyword (Task 74) =====
    
    abstract classifier Base {
        feature name: String;
    }
    
    classifier Derived {
        feature derivedField: Integer;
    }
    
    subclassifier Derived specializes Base;
    
    // ===== Multiple Typing with Comma (Task 78) =====
    
    abstract classifier Type1 { }
    abstract classifier Type2 { }
    abstract classifier Type3 { }
    
    feature MultiTypedFeature {
        // Multiple typing
        feature : Type1, Type2;
        
        // Named with multiple types
        feature multiTyped: Type1, Type2, Type3;
    }
    
    // ===== Subset/Disjoint Statements (Task 65) =====
    
    feature OccurrenceStructure {
        feature occurrences: Occurrence[*];
        feature laterOccurrence: Occurrence[0..1];
        feature earlierOccurrence: Occurrence[0..1];
        
        // Subset statement
        subset laterOccurrence.successors subsets earlierOccurrence.successors;
    }
    
    feature SetOperations {
        feature setA: Anything[*];
        feature setB: Anything[*];
        
        // Disjoint statement
        disjoint setA from setB;
    }
    
    // ===== Redefines Statements =====
    
    classifier RedefinitionBase {
        feature innerSpaceDimension: Integer default 3;
    }
    
    classifier Redefinition specializes RedefinitionBase {
        // Redefines statement with value
        redefines innerSpaceDimension = 0;
    }
}
