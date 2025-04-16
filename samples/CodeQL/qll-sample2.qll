/**
 * Source: https://github.com/github/rasmuswl-temp-ql-clone/blob/5e1c8fe8c9c3505c35f7e032169bf56d71ea2bc7/python/ql/src/semmle/python/objects/Classes.qll
 * License: Apache License 2.0
 */

import python


private import semmle.python.objects.TObject
private import semmle.python.objects.ObjectInternal
private import semmle.python.pointsto.PointsTo
private import semmle.python.pointsto.PointsToContext
private import semmle.python.pointsto.MRO
private import semmle.python.types.Builtins

/** Class representing classes */
abstract class ClassObjectInternal extends ObjectInternal {

    override string getName() {
        result = this.getClassDeclaration().getName()
    }

    /** Holds if this is a class whose instances we treat specially, rather than as a generic instance.
     * For example, `type` or `int`.
     */
    boolean isSpecial() {
        result = Types::getMro(this).containsSpecial()
    }

    /** Looks up the attribute `name` on this class.
     * Note that this may be different from `this.attr(name)`.
     * For example given the class:
     * ```class C:
     *        @classmethod
     *        def f(cls): pass
     * ```
     * `this.lookup("f")` is equivalent to `C.__dict__['f']`, which is the class-method
     *  whereas
     * `this.attr("f") is equivalent to `C.f`, which is a bound-method.
     */
    abstract predicate lookup(string name, ObjectInternal value, CfgOrigin origin);

    /** Holds if this is a subclass of the `Iterable` abstract base class. */
    boolean isIterableSubclass() {
        this = ObjectInternal::builtin("list") and result = true
        or
        this = ObjectInternal::builtin("set") and result = true
        or
        this = ObjectInternal::builtin("dict") and result = true
        or
        this != ObjectInternal::builtin("list") and
        this != ObjectInternal::builtin("set") and
        this != ObjectInternal::builtin("dict") and
        result = false
    }

    override boolean isDescriptor() { result = false }

    pragma [noinline] override predicate descriptorGetClass(ObjectInternal cls, ObjectInternal value, CfgOrigin origin) { none() }

    pragma [noinline] override predicate descriptorGetInstance(ObjectInternal instance, ObjectInternal value, CfgOrigin origin) { none() }

    pragma [noinline] override predicate binds(ObjectInternal instance, string name, ObjectInternal descriptor) {
        instance = this and
        PointsToInternal::attributeRequired(this, name) and
        this.lookup(name, descriptor, _) and
        descriptor.isDescriptor() = true
    }

    /** Approximation to descriptor protocol, skipping meta-descriptor protocol */
    pragma [noinline] override predicate attribute(string name, ObjectInternal value, CfgOrigin origin) {
        exists(ObjectInternal descriptor, CfgOrigin desc_origin |
            this.lookup(name, descriptor, desc_origin) |
            descriptor.isDescriptor() = false and
            value = descriptor and origin = desc_origin
            or
            descriptor.isDescriptor() = true and
            descriptor.descriptorGetClass(this, value, origin)
        )
    }

    override int length() { none() }

    override boolean booleanValue() { result = true }

    override boolean isClass() { result = true }

    override int intValue() {
        none()
    }

    override string strValue() {
        none()
    }

    override predicate subscriptUnknown() { none() }

    override predicate contextSensitiveCallee() { none() }

    override predicate useOriginAsLegacyObject() { none() }

    /* Classes aren't usually iterable, but can e.g. Enums */
    override ObjectInternal getIterNext() { result = ObjectInternal::unknown() }

    override predicate hasAttribute(string name) {
        this.getClassDeclaration().declaresAttribute(name)
        or
        Types::getBase(this, _).hasAttribute(name)
    }

    override predicate isNotSubscriptedType() { any() }

}
