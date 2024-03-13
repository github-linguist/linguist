trigger AccountTrigger on Account(
    before insert,
    after insert,
    before update,
    after update,
    before delete,
    after delete,
    after undelete
) {
    // This trigger utilizes a trigger handler pattern & framework.
    // For more information on how the framework operates, see the following classes:
    // * TriggerHandler.cls
    // * AccountTriggerHandler.cls

    /**
     * Our TriggerHandler framework can be invoked in one of two ways.
     * 1. You can directly invoke a trigger handler class by name using this
     * syntax: new TriggerHandlerName().run(); For instance, you could directly
     * invoke the AccountTriggerHandler().run();
     *
     */

    new AccountTriggerHandler().run();

    /**
     * 2. Alternatively, you can use the MetadataTriggerHandler().run();
     * method. This is responsible for identifying from custom metadata which
     * trigger handler classes are to be invoked, and in what order.
     *
     */

    new MetadataTriggerHandler().run();
}