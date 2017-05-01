/* -*- Mode: linguist-disable-strategy-modeline-IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://fetch.spec.whatwg.org/
 */

typedef object JSON;
// FIXME(nsm): Bug 739173: FormData is not available in workers.
// typedef (ArrayBuffer or ArrayBufferView or Blob or FormData or USVString or URLSearchParams) BodyInit;
typedef (ArrayBuffer or ArrayBufferView or Blob or USVString or URLSearchParams) BodyInit;

[NoInterfaceObject, Exposed=(Window,Worker)]
interface Body {
  readonly attribute boolean bodyUsed;
  [Throws]
  Promise<ArrayBuffer> arrayBuffer();
  [Throws]
  Promise<Blob> blob();
  // FIXME(nsm): Bug 739173 FormData is not supported in workers.
  // Promise<FormData> formData();
  [Throws]
  Promise<JSON> json();
  [Throws]
  Promise<USVString> text();
};

[NoInterfaceObject, Exposed=(Window,Worker)]
interface GlobalFetch {
  [Throws, Func="mozilla::dom::Headers::PrefEnabled"]
  Promise<Response> fetch(RequestInfo input, optional RequestInit init);
};

