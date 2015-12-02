import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;
public class InsnCount{

static HashMap<String, Integer> insnCount = new HashMap<String, Integer>();

public static void listFilesForFolder(final File folder) {
    for (final File fileEntry : folder.listFiles()) {
        if (fileEntry.isDirectory()) {
            listFilesForFolder(fileEntry);
        } else {
            if(fileEntry.getName().endsWith(".c.S"))
            {
		//System.out.println("Processing..."+fileEntry.getAbsolutePath());
		updateHashMap(fileEntry.getAbsolutePath());
	    }
        }
    }
}

static void updateHashMap(String filename)
{
    try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
    	String line;
        while ((line = br.readLine()) != null) {
		int i=0;
		//System.out.println("insn:"+line+line.length()+"a\n");	
		if(line.length() == 0 || line.charAt(0) != '\t') 	
			continue;
       		while(line.charAt(i)==' ' || line.charAt(i) == '\t')
			i++;	
		
		if(line.charAt(i) == '.')
			continue;
		else
		{
		        int end=line.indexOf('\t',i);
			if(end == -1)
				end = line.length();
			String opcode = line.substring(i,end);
			//System.out.println(opcode);
			if(insnCount.containsKey(opcode))
				insnCount.put(opcode,insnCount.get(opcode) +1);
			else
			{
				insnCount.put(opcode, 1);
			}
		}
    }
}catch(Exception e)
{
System.out.println("Exception while reading file"+e);
}

}

public static void main(String args[])
{

	final File folder = new File(".");
	listFilesForFolder(folder);
	for(Map.Entry<String, Integer> obj : insnCount.entrySet())
		System.out.println(obj.getValue()+" : "+obj.getKey());


}
}
