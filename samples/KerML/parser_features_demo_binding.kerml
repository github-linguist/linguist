/*
 * Parser Features Demo - Binding Usage
 * 
 * Demonstrates binding connector syntax patterns:
 * - Anonymous binding with multiplicity
 * - Binding with name[mult] declaration
 * - Binding with feature chain ends
 * - Binding with 'of' keyword
 */

package demo_binding {
	
	// Basic binding patterns
	feature basicBindings {
		doc /* Simple binding declarations */
		
		feature targetValue[1];
		feature featureA[1];
		feature featureB[1];
		
		// Anonymous binding with multiplicity
		binding [1] featureA = targetValue;
		
		// Named binding
		binding linkAToB of [1] featureA = [1] featureB;
	}
	
	// Advanced binding: name[mult] with source (from ShapeItems.sysml)
	feature advancedBindings {
		doc /* 
		 * Pattern: binding name[mult] of [mult] source = [mult] value
		 * This pattern was a key fix in Task 54.
		 */
		
		feature base {
			feature edges[0..*];
		}
		feature boundaryEdges[0..*];
		feature system {
			feature outputs[0..*];
		}
		feature monitor {
			feature inputs[0..*];
		}
		
		binding edgeBinding [1] of [0..*] base.edges = [0..*] boundaryEdges;
		
		// Feature chain as source
		binding dataLink [1] of [0..*] system.outputs = [0..*] monitor.inputs;
	}
	
	// Binding with feature chains
	feature chainBindings {
		feature startEvent {
			feature timestamp[1];
		}
		feature endEvent {
			feature timestamp[1];
		}
		feature config {
			feature host[1];
			feature 'port'[1];
		}
		feature serverAddress[1];
		feature serverPort[1];
		
		binding startBinding of [1] startEvent.timestamp = [1] endEvent.timestamp;
		
		// Multiple bindings in sequence
		binding [1] config.host = serverAddress;
		binding [1] config.'port' = serverPort;
	}
	
	// Real-world pattern from stdlib
	struct Shape {
		feature base {
			feature edges[0..*];
		}
		
		feature boundaryEdges[0..*];
		
		// Bind collection elements
		binding edgeBinding [1] of [0..*] base.edges = [0..*] boundaryEdges {
			doc /* Binds each edge in base to boundary */
		}
	}
}
