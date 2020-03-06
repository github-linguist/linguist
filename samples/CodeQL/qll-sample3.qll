/**
 * Source: https://github.com/github/rasmuswl-temp-ql-clone/blob/5e1c8fe8c9c3505c35f7e032169bf56d71ea2bc7/python/ql/src/semmle/python/objects/Descriptors.qll
 * License: Apache License 2.0
 */

private class PropertySetterOrDeleter extends ObjectInternal, TPropertySetterOrDeleter {

    override string toString() {
        result = this.getProperty().toString() + "." + this.getName()
    }

    override string getName() {
        this = TPropertySetterOrDeleter(_, result)
    }

    PropertyInternal getProperty() {
        this = TPropertySetterOrDeleter(result, _)
    }

    override predicate callResult(ObjectInternal obj, CfgOrigin origin) {
        exists(ControlFlowNode call |
            obj = this.getProperty() and obj = TProperty(call, _, _) and
            origin = CfgOrigin::fromCfgNode(call)
        )
    }

    override predicate introducedAt(ControlFlowNode node, PointsToContext context) {
        none()
    }

    override ClassDecl getClassDeclaration() { none() }

    override boolean isClass() { result = false }

    override ObjectInternal getClass() {
       result = TBuiltinClassObject(Builtin::special("MethodType"))
    }

    override predicate notTestableForEquality() { none() }

    override Builtin getBuiltin() { none() }

    override ControlFlowNode getOrigin() { none() }

    override predicate callResult(PointsToContext callee, ObjectInternal obj, CfgOrigin origin) { none() }

    override int intValue() { none() }

    override string strValue() { none() }

    override boolean booleanValue() { result = true }

    override predicate calleeAndOffset(Function scope, int paramOffset) { none() }

    override predicate attribute(string name, ObjectInternal value, CfgOrigin origin) { none() }

    override predicate attributesUnknown() { none() }

    override predicate subscriptUnknown() { none() }

    override boolean isDescriptor() { result = true }

    override int length() { none() }

    override predicate binds(ObjectInternal cls, string name, ObjectInternal descriptor) { none() }

    override predicate contextSensitiveCallee() { none() }

    override ObjectInternal getIterNext() { none() }

    override predicate descriptorGetClass(ObjectInternal cls, ObjectInternal value, CfgOrigin origin) { none() }

    override predicate descriptorGetInstance(ObjectInternal instance, ObjectInternal value, CfgOrigin origin) { none() }

    override predicate useOriginAsLegacyObject() { none() }

    override predicate isNotSubscriptedType() { any() }

}
