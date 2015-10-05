using System;

namespace MongoDB.Serialization.Descriptors
{
    internal class BsonPropertyValue
    {
        public bool IsDictionary { get; private set; }

        public Type Type { get; private set; }

        public object Value { get; private set; }

        public BsonPropertyValue(Type type, object value, bool isDictionary)
        {
            Type = type;
            Value = value;
            IsDictionary = isDictionary;
        }
    }
}