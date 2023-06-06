# A berry schema validator similar to a json schema validator

import persist

var sv = module('sv')

sv.regex = 'regex'
sv.time = 'time'

def gettype(data)
    var t = type(data)
    return t == 'instance' ? classname(data) : t
end

class PatternMatch
    static formats = {}
    static def load()
        if persist.has('sv_formats')
            PatternMatch.formats = persist.sv_formats
        else
            persist.sv_formats = {
                "%H:%M": {
                    "validator": "time",
                    "pattern": "%H:%M"
                }
            }
            PatternMatch.formats = persist.sv_formats
        end
    end
end

class FormatValidator
    var _compiled
    def init()
        self._compiled = {}
    end
    def match(format, value)
        var found = PatternMatch.formats.find(format)
        if found != nil
            var engine = self.invoke(found['validator'])
            if engine !=nil 
                return !!engine(self, found['pattern'], value)
            end
        end
        return !!self.regex(format, value)
    end
    def invoke(engine)
        return {
            sv.regex: self.regex,
            sv.time: self.strptime
        }.find(engine)
    end
    def regex(pattern, value)
        import re
        # If the compiled regex is not cached, compile and cache
        if !self._compiled.contains(pattern)
            self._compiled[pattern] = re.compile(pattern)
        end
        return self._compiled[pattern].match(value)
    end
    def strptime(pattern, value)
        var t = tasmota.strptime(value, pattern)
        if t && t['hour'] < 24 && t['min'] < 60 && t['sec'] < 60
            return true
        else
            return false
        end
    end
end
   
class Validate
    var node, name, path, errors, value, fv
    def addError(error)
        var path = self.path[1..]
        path.push(self.name)
        path = path.concat('.')
        self.errors[path] = error
    end
    def isValid(node, data, name, value, path, errors)
        self.node = node
        self.name = name
        self.path = path
        self.errors = errors
        self.value = value
        if !self.isValidRequired()
            self.addError('is required')
            return false
        end
        if self.value == 'undefined'
            return true
        end
        var tv = gettype(self.value)
        if !self.isValidType(tv)
            self.addError('type must be ' .. self.node['type'])
            return false;
        end
        if !self.isValidSize(tv)
            self.addError('size must be '.. self.node['size'])
            return false
        end
        if !self.isValidValues(tv)
            self.addError('values must be '.. self.node['values'])
            return false
        end
        if !self.isValidFormat() 
            self.addError('value does not match '.. self.node['format'])
            return false
        end
        return true
    end
    def isValidRequired()
        var required = self.node.find('required')
        return !(self.value == 'undefined' && required)
    end
    def isValidType(tv)
        if !self.node.contains('type')
            return true
        end
        return tv == self.node['type']
    end
    def isValidSize(tv)
        if !self.node.contains('size') 
            return true
        end
        if tv != 'list' && tv != 'string'
            return true
        end
        var sizeType = gettype(self.node['size'])
        if sizeType == 'int'
            return size(self.value) == self.node['size']
        elif sizeType == 'range'
            return size(self.value) >= self.node['size'].lower() &&
            size(self.value) <= self.node['size'].upper()
        else
            self.addError('schema size must be int or range')
            return false
        end
    end
    def isValidValues(tv)
        if !self.node.contains('values')
            return true
        end
        var nv = self.node['values']
        if gettype(nv) != 'list'
            self.addError("schema values must be list")
            return false
        end
        var num = (tv == 'int' || tv == 'real')
        for v: nv
          if num && isinstance(v, range) 
            && self.value >= v.lower() 
            && self.value <= v.upper()
            return true
          elif self.value == v
            return true
          end
        end
        return false
    end
    def isValidFormat()
        if !self.node.contains('format')
            return true
        end
        # Lazy instantiate the FormatValidator if needed
        if !self.fv self.fv = FormatValidator() end
        return self.fv.match(self.node['format'], self.value)
    end
end

class SchemaValidator
    var data, errors, path, val
    def init(schema, data)
        self.data = {}
        self.errors = {}
        self.path = []
        self.val = Validate()
        self.visit({"root": schema}, {"root": data})
    end       
    def visit(parentNode, data, opt, cleanedData)
        data = data ? data : {}
        opt = opt ? opt : {}
        cleanedData = cleanedData ? cleanedData : {}
        if opt.contains('nodeName')
            self.path.push(opt['nodeName'])
        end        
        for name: parentNode.keys()
            var node = parentNode[name]
            var nodeType = gettype(node)
            # Expand schema list shortcut
            if nodeType == 'list'
                node = {
                    "items": node[0],
                    "type": 'list'
                }
                if node['items'].contains('size')
                    node['size'] = node['items']['size']
                    node['items'].remove('size')
                end
            end
            var value = data.contains(name) ? data[name] : 'undefined'
            if !self.val.isValid(node, data, name, value, self.path, self.errors)
                continue
            end
            if value == 'undefined'
                continue
            end
            if node['type'] == 'map'
                cleanedData[name] = {}
                self.visit(
                    node['properties'], 
                    data[name], 
                    {"nodeName": name}, 
                    cleanedData[name]
                )
                continue
            elif node['type'] == 'list'
                cleanedData[name] = []
                for idx: data[name].keys()
                    self.visit(
                        {idx: node['items']},
                        {idx: data[name][idx]},
                        {"nodeName": name},
                        cleanedData[name]
                    )
                end
                continue
            end
            if gettype(cleanedData) == 'list'
                cleanedData.push(data[name])
            else
                cleanedData[name] = data[name]
            end
        end
        if opt.contains('nodeName')
            self.path.pop()
        end
        if self.path.size() == 0
            self.data = cleanedData.find('root')
        end
        return self
    end
    def result()
        return {
            "is_valid": !self.errors.size(),
            "errors": self.errors,
            "data": self.data
        }
    end
end

# Load persisted regex patterns
PatternMatch.load()

sv.formats = def() 
    var l = []
    for k: PatternMatch.formats.keys()
        l.push(k)
    end
    return l 
end
sv.add_format = def(pattern, engine, value)
    if type(pattern) != 'string' return end
    if type(value) != 'string' return end
    if engine != sv.regex && engine != sv.time
        return
    end
    PatternMatch.formats.setitem(
        value, {"validator": engine, "pattern": pattern}
    )
    persist.save()
end
sv.remove_format = def(key)
    PatternMatch.formats.remove(key)
    persist.save()
end
sv.validate = def(schema, data) 
    return SchemaValidator(schema, data).result() 
end
return sv