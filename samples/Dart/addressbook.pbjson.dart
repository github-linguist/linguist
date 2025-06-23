//
//  Generated code. Do not modify.
//  source: addressbook.proto
//
// @dart = 3.3

// ignore_for_file: annotate_overrides, camel_case_types, comment_references
// ignore_for_file: constant_identifier_names
// ignore_for_file: curly_braces_in_flow_control_structures
// ignore_for_file: deprecated_member_use_from_same_package, library_prefixes
// ignore_for_file: non_constant_identifier_names

import 'dart:convert' as $convert;
import 'dart:core' as $core;
import 'dart:typed_data' as $typed_data;

@$core.Deprecated('Use personDescriptor instead')
const Person$json = {
  '1': 'Person',
  '2': [
    {'1': 'name', '3': 1, '4': 2, '5': 9, '10': 'name'},
    {'1': 'id', '3': 2, '4': 2, '5': 5, '10': 'id'},
    {'1': 'email', '3': 3, '4': 1, '5': 9, '10': 'email'},
    {'1': 'phone', '3': 4, '4': 3, '5': 11, '6': '.tutorial.Person.PhoneNumber', '10': 'phone'},
  ],
  '3': [Person_PhoneNumber$json],
  '4': [Person_PhoneType$json],
};

@$core.Deprecated('Use personDescriptor instead')
const Person_PhoneNumber$json = {
  '1': 'PhoneNumber',
  '2': [
    {'1': 'number', '3': 1, '4': 2, '5': 9, '10': 'number'},
    {'1': 'type', '3': 2, '4': 1, '5': 14, '6': '.tutorial.Person.PhoneType', '7': 'HOME', '10': 'type'},
  ],
};

@$core.Deprecated('Use personDescriptor instead')
const Person_PhoneType$json = {
  '1': 'PhoneType',
  '2': [
    {'1': 'MOBILE', '2': 0},
    {'1': 'HOME', '2': 1},
    {'1': 'WORK', '2': 2},
  ],
};

/// Descriptor for `Person`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List personDescriptor = $convert.base64Decode(
    'CgZQZXJzb24SEgoEbmFtZRgBIAIoCVIEbmFtZRIOCgJpZBgCIAIoBVICaWQSFAoFZW1haWwYAy'
    'ABKAlSBWVtYWlsEjIKBXBob25lGAQgAygLMhwudHV0b3JpYWwuUGVyc29uLlBob25lTnVtYmVy'
    'UgVwaG9uZRpbCgtQaG9uZU51bWJlchIWCgZudW1iZXIYASACKAlSBm51bWJlchI0CgR0eXBlGA'
    'IgASgOMhoudHV0b3JpYWwuUGVyc29uLlBob25lVHlwZToESE9NRVIEdHlwZSIrCglQaG9uZVR5'
    'cGUSCgoGTU9CSUxFEAASCAoESE9NRRABEggKBFdPUksQAg==');

@$core.Deprecated('Use addressBookDescriptor instead')
const AddressBook$json = {
  '1': 'AddressBook',
  '2': [
    {'1': 'person', '3': 1, '4': 3, '5': 11, '6': '.tutorial.Person', '10': 'person'},
  ],
};

/// Descriptor for `AddressBook`. Decode as a `google.protobuf.DescriptorProto`.
final $typed_data.Uint8List addressBookDescriptor = $convert.base64Decode(
    'CgtBZGRyZXNzQm9vaxIoCgZwZXJzb24YASADKAsyEC50dXRvcmlhbC5QZXJzb25SBnBlcnNvbg'
    '==');

