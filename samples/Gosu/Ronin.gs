/**
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
 */

package ronin

uses gw.util.concurrent.LockingLazyVar
uses gw.lang.reflect.*
uses java.lang.*
uses java.io.*
uses ronin.config.*
uses org.slf4j.*

/**
 *  The central location for Ronin utility methods.  Controllers and templates should generally access the
 *  methods and properties they inherit from {@link ronin.IRoninUtils} instead of using the methods and
 *  properties here.
 */
class Ronin {

  // One static field to rule the all...
  static var _CONFIG : IRoninConfig as Config

  // And one thread local to bind them
  static var _CURRENT_REQUEST = new ThreadLocal<RoninRequest>();

  // That's inconstructable
  private construct() {}

  internal static function init(servlet : RoninServlet, m : ApplicationMode, src : File) {
    if(_CONFIG != null) {
      throw "Cannot initialize a Ronin application multiple times!"
    }
    var cfg = TypeSystem.getByFullNameIfValid("config.RoninConfig")
    var defaultWarning = false
    if(cfg != null) {
      var ctor = cfg.TypeInfo.getConstructor({ronin.config.ApplicationMode, ronin.RoninServlet})
      if(ctor == null) {
        throw "config.RoninConfig must have a constructor with the same signature as ronin.config.RoninConfig"
      }
      _CONFIG = ctor.Constructor.newInstance({m, servlet}) as IRoninConfig
    } else {
      _CONFIG = new DefaultRoninConfig(m, servlet)
      defaultWarning = true
    }
    var roninLogger = TypeSystem.getByFullNameIfValid("ronin.RoninLoggerFactory")
    if(roninLogger != null) {
      roninLogger.TypeInfo.getMethod("init", {ronin.config.LogLevel}).CallHandler.handleCall(null, {LogLevel})
    }
    if(defaultWarning) {
      log("No configuration was found at config.RoninConfig, using the default configuration...", :level=WARN)
    }
    Quartz.maybeStart()
    ReloadManager.setSourceRoot(src)
  }

  internal static property set CurrentRequest(req : RoninRequest) {
    _CURRENT_REQUEST.set(req)
  }

  //============================================
  // Public API
  //============================================

  /**
   *  The trace handler for the current request.
   */
  static property get CurrentTrace() : Trace {
    return CurrentRequest?.Trace
  }

  /**
   *  Ronin's representation of the current request.
   */
  static property get CurrentRequest() : RoninRequest {
    return _CURRENT_REQUEST.get()
  }

  /**
   *  The mode in which this application is running.
   */
  static property get Mode() : ApplicationMode {
    return _CONFIG?.Mode ?: TESTING
  }

  /**
   *  The log level at and above which log messages should be displayed.
   */
  static property get LogLevel() : LogLevel {
    return _CONFIG?.LogLevel ?: DEBUG
  }

  /**
   *  Whether or not to display detailed trace information on each request.
   */
  static property get TraceEnabled() : boolean {
    return _CONFIG != null ? _CONFIG.TraceEnabled : true
  }

  /**
   *  The default controller method to call when no method name is present in the request URL.
   */
  static property get DefaultAction() : String {
    return _CONFIG?.DefaultAction
  }

  /**
   *  The default controller to call when no controller name is present in the request URL.
   */
  static property get DefaultController() : Type {
    return _CONFIG?.DefaultController
  }

  /**
   *  The servlet responsible for handling Ronin requests.
   */
  static property get RoninServlet() : RoninServlet {
    return _CONFIG?.RoninServlet
  }

  /**
   *  The handler for request processing errors.
   */
  static property get ErrorHandler() : IErrorHandler {
    return _CONFIG?.ErrorHandler
  }

  /**
   *  The custom handler for logging messages.
   */
  static property get LogHandler() : ILogHandler {
    return _CONFIG?.LogHandler
  }

  /**
   *  Logs a message using the configured log handler.
   *  @param msg The text of the message to log, or a block which returns said text.
   *  @param level (Optional) The level at which to log the message.
   *  @param component (Optional) The logical component from whence the message originated.
   *  @param exception (Optional) An exception to associate with the message.
   */
  static function log(msg : Object, level : LogLevel = null, component : String = null, exception : java.lang.Throwable = null) {
    if(level == null) {
      level = INFO
    }
    if(LogLevel <= level) {
      var msgStr : String
      if(msg typeis block():String) {
        msgStr = (msg as block():String)()
      } else {
        msgStr = msg as String
      }
      if(_CONFIG?.LogHandler != null) {
        _CONFIG.LogHandler.log(msgStr, level, component, exception)
      } else {
        switch(level) {
          case TRACE:
            LoggerFactory.getLogger(component?:Logger.ROOT_LOGGER_NAME).trace(msgStr, exception)
            break
          case DEBUG:
            LoggerFactory.getLogger(component?:Logger.ROOT_LOGGER_NAME).debug(msgStr, exception)
            break
          case INFO:
            LoggerFactory.getLogger(component?:Logger.ROOT_LOGGER_NAME).info(msgStr, exception)
            break
          case WARN:
            LoggerFactory.getLogger(component?:Logger.ROOT_LOGGER_NAME).warn(msgStr, exception)
            break
          case ERROR:
          case FATAL:
            LoggerFactory.getLogger(component?:Logger.ROOT_LOGGER_NAME).error(msgStr, exception)
            break
        }
      }
    }
  }

  /**
   *  The caches known to Ronin.
   */
  static enum CacheStore {
    REQUEST,
    SESSION,
    APPLICATION
  }

  /**
   *  Retrieves a value from a cache, or computes and stores it if it is not in the cache.
   *  @param value A block which will compute the desired value.
   *  @param name (Optional) A unique identifier for the value.  Default is null, which means one will be
   *  generated from the type of the value.
   *  @param store (Optional) The cache store used to retrieve or store the value.  Default is the request cache.
   *  @return The retrieved or computed value.
   */
  static function cache<T>(value : block():T, name : String = null, store : CacheStore = null) : T {
    if(store == null or store == REQUEST) {
      return _CONFIG.RequestCache.getValue(value, name)
    } else if (store == SESSION) {
      return _CONFIG.SessionCache.getValue(value, name)
    } else if (store == APPLICATION) {
      return _CONFIG.ApplicationCache.getValue(value, name)
    } else {
      throw "Don't know about CacheStore ${store}"
    }
  }

  /**
   *  Invalidates a cached value in a cache.
   *  @param name The unique identifier for the value.
   *  @param store The cache store in which to invalidate the value.
   */
  static function invalidate<T>(name : String, store : CacheStore) {
    if(store == null or store == REQUEST) {
      _CONFIG.RequestCache.invalidate(name)
    } else if (store == SESSION) {
      _CONFIG.SessionCache.invalidate(name)
    } else if (store == APPLICATION) {
      _CONFIG.ApplicationCache.invalidate(name)
    } else {
      throw "Don't know about CacheStore ${store}"
    }
  }


  /**
   *  Detects changes made to resources in the Ronin application and
   *  reloads them.  This function should only be called when Ronin is
   *  in development mode.
   */
  static function loadChanges() {
    ReloadManager.detectAndReloadChangedResources() 
  }

}
