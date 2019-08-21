/* @generated from adl module common */

import * as ADL from './runtime/adl';
import * as common_strings from './common/strings';

/**
 * A instant in time, represented as milliseconds from
 * the epoch of "1970-01-01T00:00:00Z
 */
export type Instant = number;

const Instant_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}}},"name":"Instant","version":{"kind":"nothing"}}};

export function texprInstant(): ADL.ATypeExpr<Instant> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "Instant"}}, parameters : []}};
}

/**
 * A date in ISO8601 format
 */
export type LocalDate = string;

const LocalDate_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"1970-01-01"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalDate","version":{"kind":"nothing"}}};

export function texprLocalDate(): ADL.ATypeExpr<LocalDate> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "LocalDate"}}, parameters : []}};
}

/**
 * A time in ISO8601 format
 */
export type LocalTime = string;

const LocalTime_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"00:00:00"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalTime","version":{"kind":"nothing"}}};

export function texprLocalTime(): ADL.ATypeExpr<LocalTime> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "LocalTime"}}, parameters : []}};
}

/**
 * A datetime in ISO8601 format
 */
export type LocalDateTime = string;

const LocalDateTime_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"1970-01-01T00:00:00"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalDateTime","version":{"kind":"nothing"}}};

export function texprLocalDateTime(): ADL.ATypeExpr<LocalDateTime> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "LocalDateTime"}}, parameters : []}};
}

/**
 * An IANA timezone
 */
export type Timezone = common_strings.StringNE;

const Timezone_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}},"name":"Timezone","version":{"kind":"nothing"}}};

export function texprTimezone(): ADL.ATypeExpr<Timezone> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "Timezone"}}, parameters : []}};
}

/**
 * A holder for paginated results
 */
export interface Paginated<T> {
  /**
   * The paginated items
   */
  items: T[];
  /**
   * The offset used for this query
   */
  current_offset: number;
  /**
   * The size of the entire date set
   */
  total_size: number;
}

export function makePaginated<T>(
  input: {
    items: T[],
    current_offset: number,
    total_size: number,
  }
): Paginated<T> {
  return {
    items: input.items,
    current_offset: input.current_offset,
    total_size: input.total_size,
  };
}

const Paginated_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"items","default":{"kind":"nothing"},"name":"items","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"current_offset","default":{"kind":"nothing"},"name":"current_offset","typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}},{"annotations":[],"serializedName":"total_size","default":{"kind":"nothing"},"name":"total_size","typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}}]}},"name":"Paginated","version":{"kind":"nothing"}}};

export function texprPaginated<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Paginated<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "Paginated"}}, parameters : [texprT.value]}};
}

/**
 * Empty Struct (Used mostly for Void RPC responses)
 */
export interface Unit {
}

export function makeUnit(
  _input: {
  }
): Unit {
  return {
  };
}

const Unit_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[]}},"name":"Unit","version":{"kind":"nothing"}}};

export function texprUnit(): ADL.ATypeExpr<Unit> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "Unit"}}, parameters : []}};
}

/**
 * Phantom type to capture a StringMap with a named string key type:
 */
export type StringKeyMap<_K, V> = {[key: string]: V};

const StringKeyMap_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["K","V"],"typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"typeParam","value":"V"},"parameters":[]}]}}},"name":"StringKeyMap","version":{"kind":"nothing"}}};

export function texprStringKeyMap<K, V>(texprK : ADL.ATypeExpr<K>, texprV : ADL.ATypeExpr<V>): ADL.ATypeExpr<StringKeyMap<K, V>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "StringKeyMap"}}, parameters : [texprK.value, texprV.value]}};
}

/**
 * Naming aid for strings used as keys
 */
export type Key<_T> = string;

const Key_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["T"],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Key","version":{"kind":"nothing"}}};

export function texprKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Key<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "Key"}}, parameters : [texprT.value]}};
}

/**
 * A value of type T along with the Key<T>
 */
export interface WithKey<T> {
  key: Key<T>;
  value: T;
}

export function makeWithKey<T>(
  input: {
    key: Key<T>,
    value: T,
  }
): WithKey<T> {
  return {
    key: input.key,
    value: input.value,
  };
}

const WithKey_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"key","default":{"kind":"nothing"},"name":"key","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Key"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"WithKey","version":{"kind":"nothing"}}};

export function texprWithKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<WithKey<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "WithKey"}}, parameters : [texprT.value]}};
}

/**
 * Postgres Geography type that is serialized using GeoJson
 */
export type GeographyGeoJson = string;

const GeographyGeoJson_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"GeographyGeoJson","version":{"kind":"nothing"}}};

export function texprGeographyGeoJson(): ADL.ATypeExpr<GeographyGeoJson> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "GeographyGeoJson"}}, parameters : []}};
}

/**
 * Postgres Geometry type
 */
export type GeometryWKT = string;

const GeometryWKT_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"GeometryWKT","version":{"kind":"nothing"}}};

export function texprGeometryWKT(): ADL.ATypeExpr<GeometryWKT> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "common",name : "GeometryWKT"}}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.Instant" : Instant_AST,
  "common.LocalDate" : LocalDate_AST,
  "common.LocalTime" : LocalTime_AST,
  "common.LocalDateTime" : LocalDateTime_AST,
  "common.Timezone" : Timezone_AST,
  "common.Paginated" : Paginated_AST,
  "common.Unit" : Unit_AST,
  "common.StringKeyMap" : StringKeyMap_AST,
  "common.Key" : Key_AST,
  "common.WithKey" : WithKey_AST,
  "common.GeographyGeoJson" : GeographyGeoJson_AST,
  "common.GeometryWKT" : GeometryWKT_AST
};
