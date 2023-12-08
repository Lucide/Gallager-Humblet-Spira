from utils_yamljson import load



class Rules:
    rules = []

    def __init__(self):
        pass

    def __init__(self, filename):
        self.rules = load(filename)

        # check well-formedness of rules
        for rule in self.rules:
            # checks that rules contain 'if' and 'then' clauses
            assert 'if' in rule and 'then' in rule, \
                   f"Rule {rule} must have 'if' and 'then' clauses."
            if rule['if'] == None:
                rule['if'] = {}
            if rule['then'] == None:
                rule['then'] = {}
            assert isinstance(rule['if'], dict) and isinstance(rule['then'], dict), \
                   f"Rule {rule} must have 'if' and 'then' clauses as associative arrays."

            # find free variables in 'if' clause and store corresponding keys
            rule['var2key'] = {}
            for key, value in rule['if'].items():
                if isinstance(value, str) and value.startswith('$') and value not in rule['var2key']:
                    rule['var2key'][value] = key

            # check that every free variable in 'then' clause also appeared in 'if' clause
            for key, value in rule['then'].items():
                if isinstance(value, str) and value.startswith('$'):
                    assert value in rule['var2key'], \
                           f"Rule {rule} contains unbound variable {value} in 'then' clause."


    def complete(self, data):
        for rule in self.rules:
            if_clause = rule['if'].copy()
            then_clause = rule['then'].copy()
            applicable = True
            
            # instantiate every variable with a corresponding value in data, if any
            var2value = {}
            for var, key in rule['var2key'].items():
                if key in data:
                    var2value[var] = data[key]
                else:
                    var2value[var] = None

            # replaces all variables appearing in if_clause and in then_clause
            # with their instances, so as to obtain a ground rule
            for var, value in var2value.items():
                for key, value2 in if_clause.items():
                    if value2 == var:
                        if_clause[key] = value
                for key, value2 in then_clause.items():
                    if value2 == var:
                        then_clause[key] = value

            # check if ground rule is applicable: this happens when
            # 1) all key-value pairs in if_clause match with key-value pairs in data
            # 2) all variables have been instantiated
            applicable = all((key in data and data[key] == value) \
                             or (key not in data and value == None) \
                             for key, value in if_clause.items()) \
                         and \
                         all(value != None for var, value in var2value.items())
                         
            # apply ground rule
            if applicable:
                for key, value in then_clause.items():
                    # in the particular case where key exists in data and
                    # its value and the corresponding value in then_clause are both of type dict,
                    # we take the union of the two associative arrays
                    if key in data and isinstance(value, dict) and isinstance(data[key], dict):
                        data[key] = {**value, **data[key]}
                    # otherwise we set value in data only if key is not already present
                    elif key not in data:
                        data[key] = value  
