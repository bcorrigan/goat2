package org.goat.jcalc;

import java.util.Hashtable;
import java.math.*;

public class VariableTable {
    private Hashtable variables = new Hashtable();

    //pi and e, others may be added later.
    private Hashtable variable_variables = new Hashtable();
    public VariableTable(int scl) throws InterruptedException {
        variables.put("true",  Boolean.valueOf(true));
        variables.put("false", Boolean.valueOf(false));
        variables.put("brian", BigDecimal.valueOf(25));
        
        PI p_class = new PI(scl);
        E e_class = new E(scl);
        //E e_class = new E(scl);
        variable_variables.put("pi", p_class);
        variable_variables.put("e", e_class);
    }
 
    public boolean isVariable(String s) throws InterruptedException {
        if(variables.containsKey(s) ||
           variable_variables.containsKey(s)
        ){
            return true;
        }
        return false;
    }
    
    public Object variableValue(String s, int scl) throws InterruptedException {
        if(variables.containsKey(s)){
            return variables.get(s);
        }else{
            variable_interface vio = (variable_interface)variable_variables.get(s);
            return vio.getValue(scl);
            //return null;
        }
    }
 
    
    
}
