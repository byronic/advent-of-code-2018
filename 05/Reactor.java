import java.util.Map;
import java.util.HashMap;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;


public class Reactor {
  private final List<String> alpha = Arrays.asList("[Aa]", "[Bb]", "[Cc]", "[Dd]", "[Ee]", "[Ff]", "[Gg]", "[Hh]", "[Ii]", "[Jj]", "[Kk]", "[Ll]", "[Mm]", "[Nn]", "[Oo]", "[Pp]", "[Qq]", "[Rr]", "[Ss]", "[Tt]", "[Uu]", "[Vv]", "[Ww]", "[Xx]", "[Yy]", "[Zz]");
  private final List<String> lines;

  public Reactor(String file) throws IOException {
    lines = Files.readAllLines(Paths.get(file));
  }

  // I mainly did this so I wouldn't have to hold onto a temporary minimum int during the improvedReact loop
  private int stupidMin(int a, int b) {
    if (a < b) {
      return a;
    }
    return b;
  }

  public int improvedReact() {
    int minLength = lines.get(0).length();
    for(String letter : alpha) {
      String temp = lines.get(0).replaceAll(letter, "");
      System.out.print(String.format("Replaced %s, checking a length %d string", letter, temp.length()));
      minLength = stupidMin(minLength, reactHelper(temp).length());
      System.out.println(String.format("; minimum so far: %d", minLength));
    }
    return minLength;
  }

  private String reactHelper(String s) {
    String result = s;
    int lastLength = -1;
    while(result.length() != lastLength) {
      lastLength = result.length();
      result = result.replaceAll(
            "(Aa)|(Bb)|(Cc)|(Dd)|(Ee)|(Ff)|(Gg)|(Hh)|(Ii)|(Jj)|(Kk)|(Ll)|(Mm)|(Nn)|(Oo)|(Pp)|(Qq)|(Rr)|(Ss)|(Tt)|(Uu)|(Vv)|(Ww)|(Xx)|(Yy)|(Zz)|(aA)|(bB)|(cC)|(dD)|(eE)|(fF)|(gG)|(hH)|(iI)|(jJ)|(kK)|(lL)|(mM)|(nN)|(oO)|(pP)|(qQ)|(rR)|(sS)|(tT)|(uU)|(vV)|(wW)|(xX)|(yY)|(zZ)",
            "");
    }
    return result;
  }

  public int react() {
    System.out.println("Processing first react()");
    return reactHelper(lines.get(0)).length();
  }

  public static void main(String[] args) throws Exception {
    Reactor r = new Reactor("dataset");
    System.out.println(String.format("react() result: %d", r.react()));
    System.out.println(r.improvedReact());
  }
}
