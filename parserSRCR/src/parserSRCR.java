import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;

public class parserSRCR {
    public static void main(String[] argV){
        String csvFile= null;
        String prologOut = null;
        String parse = null;
        String cvsSplitBy = ";";
        for(int i=0; i<argV.length;i++){
            switch (argV[i]){
                case "-parse": parse =argV[i+1]; i++;break;
                case "-in": csvFile ="../"+argV[i+1]; i++;break;
                case "-out": prologOut = "../"+ argV[i+1];i++;break;
                case "-sep": cvsSplitBy =argV[i+1];i++;break;
            }
        }


        String r ;
        ArrayList<String> ret = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader(csvFile));
            switch (parse){

                case "paragens":{
                    reader.readLine();
                    while ((r = reader.readLine()) != null) {
                        String[] line = r.split(cvsSplitBy);
                        StringBuilder ready= new StringBuilder();
                        ready.append("paragem(");
                        //Gid
                        ready.append(Integer.parseInt(line[0]));ready.append(",");
                        //Lat
                        ready.append(Double.parseDouble(line[1]));ready.append(",");
                        //Long
                        ready.append(Double.parseDouble(line[2]));ready.append(",");
                        //Estado
                        ready.append("\"" +line[3]+"\"");ready.append(",");
                        //Tipo
                        ready.append("\""+line[4]+"\"");ready.append(",");
                        //Publicidade
                        ready.append("\""+line[5]+"\"");ready.append(",");
                        //Operadora
                        ready.append("\""+line[6]+"\"");ready.append(",");
                        //Carreira(s)
                        try {
                            String[] cars = line[7].split(",");
                            StringBuilder carreiras= new StringBuilder("["+cars[0]);
                            cars[0]=null;
                            for (String s : cars){
                                if (s != null) {
                                    carreiras.append(",");
                                    carreiras.append(Integer.parseInt(s));
                                }
                            }
                            carreiras.append("]");
                            ready.append(carreiras.toString());ready.append(",");
                        }catch (Exception e){e.printStackTrace();System.out.println(line[7]);return;}

                        //Cod de rua
                        ready.append(Integer.parseInt(line[8]));ready.append(",");
                        //Nome rua
                        ready.append("\""+line[9]+"\"");ready.append(",");
                        //Freguesia
                        ready.append("\""+line[10]+"\"");
                        ready.append(").");
                        ret.add(ready.toString());
                        System.out.println("-----------------------------------------------");
                        System.out.println("| AVISO : NÃO ESQUECER DE RETIRAR TODOS OS 'Á'|");
                        System.out.println("-----------------------------------------------");
                    }
                break;
                }
                case "adjacencia":{
                    int carreira=0;
                    HashSet<Integer> nodes = new HashSet<>();
                    String aresta;
                    int lastGid = -1;
                    try{
                        StringBuilder adjList = new StringBuilder();
                        ret.add("grafo(");

                    while ((r = reader.readLine()) != null) {
                        String[] line = r.split(cvsSplitBy);
                        int i =(int) Double.parseDouble(line[7]);
                        int gid =(int) Double.parseDouble(line[0]);

                        if(i!=carreira){
                            carreira=i; System.out.println(carreira);
                        }else{
                            aresta="aresta("+lastGid+","+gid+"),"; ret.add(aresta);
                        }
                        nodes.add(gid);
                        lastGid=gid;
                    }
                    String tmp =ret.remove(ret.size()-1);
                           tmp = tmp.substring(0,tmp.length()-1);ret.add(tmp);
                    ret.add("]");
                    StringBuilder nds = new StringBuilder("[");
                    for(Integer n : nodes){nds.append(n);nds.append(",");}
                        nds.delete(nds.length()-1,nds.length());
                    nds.append("],[");
                    ret.add(1, nds.toString());
                    ret.add(").");
                    }catch(Exception e){e.printStackTrace();}
                    }
                    break;
            }


            reader.close();
            //System.out.println(ret);
            BufferedWriter writer = new BufferedWriter(new FileWriter(prologOut));
            for(String s : ret) {writer.write(s);writer.newLine(); }
            writer.flush();
            writer.close();
            System.out.println("Parsed "+ (ret.size() +(parse=="paragens"? 0: -2)) +" lines\n" );
        }catch (Exception e){e.printStackTrace();}







    }
}
