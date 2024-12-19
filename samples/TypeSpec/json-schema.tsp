import "@typespec/json-schema";

using TypeSpec.JsonSchema;

@jsonSchema
namespace Schemas;

model Person {
  /** The person's first name. */
  firstName: string;

  /** The person's last name. */
  lastName: string;

  /** Age in years which must be equal to or greater than zero. */
  @minValue(0) age: int32;

  /** Person address */
  address: Address;

  /** List of nick names */
  @uniqueItems nickNames?: string[];

  /** List of cars person owns */
  cars?: Car[];
}

/** Respresent an address */
model Address {
  street: string;
  city: string;
  country: string;
}
model Car {
  /** Kind of car */
  kind: "ev" | "ice";

  /** Brand of the car */
  brand: string;

  /** Model of the car */
  `model`: string;
}