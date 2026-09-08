void DispatchCallback()
{
    auto context = new Runtime::Context();
    CallLocalFunction("OnReady", context);
    delete context;
}
