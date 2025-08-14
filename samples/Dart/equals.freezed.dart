// dart format width=80
// coverage:ignore-file
// GENERATED CODE - DO NOT MODIFY BY HAND
// ignore_for_file: type=lint
// ignore_for_file: unused_element, deprecated_member_use, deprecated_member_use_from_same_package, use_function_type_syntax_for_parameters, unnecessary_const, avoid_init_to_null, invalid_override_different_default_values_named, prefer_expression_function_bodies, annotate_overrides, invalid_annotation_target, unnecessary_question_mark

part of 'equals.dart';

// **************************************************************************
// FreezedGenerator
// **************************************************************************

// dart format off
T _$identity<T>(T value) => value;

/// @nodoc
mixin _$Equals {
  String? get name;
  int? get age;

  /// Create a copy of Equals
  /// with the given fields replaced by the non-null parameter values.
  @JsonKey(includeFromJson: false, includeToJson: false)
  @pragma('vm:prefer-inline')
  $EqualsCopyWith<Equals> get copyWith =>
      _$EqualsCopyWithImpl<Equals>(this as Equals, _$identity);

  @override
  String toString() {
    return 'Equals(name: $name, age: $age)';
  }
}

/// @nodoc
abstract mixin class $EqualsCopyWith<$Res> {
  factory $EqualsCopyWith(Equals value, $Res Function(Equals) _then) =
  _$EqualsCopyWithImpl;
  @useResult
  $Res call({String? name, int? age});
}

/// @nodoc
class _$EqualsCopyWithImpl<$Res> implements $EqualsCopyWith<$Res> {
  _$EqualsCopyWithImpl(this._self, this._then);

  final Equals _self;
  final $Res Function(Equals) _then;

  /// Create a copy of Equals
  /// with the given fields replaced by the non-null parameter values.
  @pragma('vm:prefer-inline')
  @override
  $Res call({
    Object? name = freezed,
    Object? age = freezed,
  }) {
    return _then(_self.copyWith(
      name: freezed == name
          ? _self.name
          : name // ignore: cast_nullable_to_non_nullable
      as String?,
      age: freezed == age
          ? _self.age
          : age // ignore: cast_nullable_to_non_nullable
      as int?,
    ));
  }
}

/// @nodoc

class _Equals extends Equals {
  _Equals({this.name, this.age}) : super._();

  @override
  final String? name;
  @override
  final int? age;

  /// Create a copy of Equals
  /// with the given fields replaced by the non-null parameter values.
  @override
  @JsonKey(includeFromJson: false, includeToJson: false)
  @pragma('vm:prefer-inline')
  _$EqualsCopyWith<_Equals> get copyWith =>
      __$EqualsCopyWithImpl<_Equals>(this, _$identity);

  @override
  String toString() {
    return 'Equals(name: $name, age: $age)';
  }
}

/// @nodoc
abstract mixin class _$EqualsCopyWith<$Res> implements $EqualsCopyWith<$Res> {
  factory _$EqualsCopyWith(_Equals value, $Res Function(_Equals) _then) =
  __$EqualsCopyWithImpl;
  @override
  @useResult
  $Res call({String? name, int? age});
}

/// @nodoc
class __$EqualsCopyWithImpl<$Res> implements _$EqualsCopyWith<$Res> {
  __$EqualsCopyWithImpl(this._self, this._then);

  final _Equals _self;
  final $Res Function(_Equals) _then;

  /// Create a copy of Equals
  /// with the given fields replaced by the non-null parameter values.
  @override
  @pragma('vm:prefer-inline')
  $Res call({
    Object? name = freezed,
    Object? age = freezed,
  }) {
    return _then(_Equals(
      name: freezed == name
          ? _self.name
          : name // ignore: cast_nullable_to_non_nullable
      as String?,
      age: freezed == age
          ? _self.age
          : age // ignore: cast_nullable_to_non_nullable
      as int?,
    ));
  }
}

// dart format on
