# Request Middleware Design

## Overview

The request middleware handles Searchex requests.

The request middleware is called `reqm`.

Search REQuest Middleware - pronounced "wreck-em" - referred 
to as 'request middleware'.

## Extensible

Request middleware comes with modules for a default search pipeline.

It is possible to create custom request modules and reconfigure the pipeline.

**Middleware Proxy**

Use `Shreq.Proxy` for configurable module invocation:

    step Shreq.Proxy, :index
    step :myfunc


**Config File**

   :request_middleware:
     :catalog: "MyCatalog"
     :index:   "MyIndex"

**Custom Request Middleware**

Custom middleware must be in the `Searchex.Request` namespace.

    Searchex.Request.MyCatalog
    Searchex.Request.MyIndex
