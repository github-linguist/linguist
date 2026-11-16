/*
 * Parser Features Demo: Advanced Bodies
 * 
 * Demonstrates advanced body features:
 * - Return statements in predicate bodies (Task 83)
 * - Anonymous return parameters (Task 85)
 * - Body params with multiplicity after type (Task 75)
 * - Shorthand body param syntax (Task 87)
 * - Bool bodies with return statements (Task 80)
 * - General members in constraint bodies (Task 91)
 * 
 * Based on real stdlib patterns
 */

package AdvancedBodies {
    
    // The library is not implicitly visible: a bare name needs an import.
    private import ScalarValues::*;
    private import Transfers::Transfer;
    private import Performances::Performance;
    private import ControlFunctions::exists;
    private import ControlFunctions::forAll;
    private import SequenceFunctions::size;
    
    // ===== Predicate with Return Statements (Task 83) =====
    
    abstract predicate TransferCheck {
        in t1: Transfer [1];
        in t2: Transfer [1];
        
        return t1Before: Boolean;
    }
    
    // ===== Anonymous Return Parameters (Task 85) =====
    
    abstract predicate BooleanEvaluation {
        doc
        /* Predicate that returns a boolean value */
        
        return : Boolean[1];
    }
    
    function CalculateSum {
        in x: Integer;
        in y: Integer;
        
        return : Integer;
    }
    
    // ===== Body Param with Multiplicity After Type (Task 75) =====
    
    function processTransfers {
        in t1: Transfer [1];
        in t2: Transfer [2..*];
        
        return result: Boolean;
    }
    
    // ===== Shorthand Body Param Syntax (Task 87) =====
    
    classifier Point { feature x: Real; feature y: Real; }
    
    feature Shape {
        feature vertices: Point[*];
        
        // Shorthand: {param : Type; expression} 
        feature hasPositive: Boolean = 
            vertices->exists{p : Point; p.x > 0};
        
        feature allPositive: Boolean = 
            vertices->forAll{v : Point; v.x >= 0};
    }
    
    // ===== Bool Bodies with Return (Task 80) =====
    
    bool earlierCheck {
        in t1: Transfer [1];
        in t2: Transfer [1];
        
        return t1First = true;
    }
    
    // Bool body with single expression
    bool simpleCheck {
        in value: Integer;
        
        value > 0
    }
    
    // ===== General Members in Predicate Bodies (Task 91) =====
    
    predicate SystemRequirements {
        doc
        /* Predicate bodies can contain feature members and an expression */
        
        // Feature declarations inside the predicate body
        feature ownedPerformances: Performance[*];
        feature nestedPerformances: Performance[*];
        
        // Result expression
        size(ownedPerformances) > 0
    }
}
