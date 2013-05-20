# doc-routes

auto-doc compojure routes.

```clojure
(GET "/foo" [id]
  {:does "returns ratings distribution across resident surveys"
   :args  ["id" "property-id"]
   :curl  "-d property-id=96975"}
  (data/get-foo id))
```

## Usage

FIXME

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
