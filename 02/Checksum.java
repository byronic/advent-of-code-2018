import java.util.Map;
import java.util.HashMap;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Checksum {
  private int checkString (String s) {
    Map<Character, Integer> map = new HashMap<Character, Integer>();
    for (char c : s.toCharArray()) {
      if (map.containsKey(c)) {
        map.put(c, map.get(c) + 1);
      } else {
        map.put(c, 1);
      }
    }
    boolean two = false;
    boolean three = false;
    for (int i : map.values()) {
      switch (i) {
      case 2: two = true;
        break;
      case 3: three = true;
        break;
      }
      if (two && three) break;
    }
    // a little throwback to my C++ days
    int result = 0;
    if (two) {
      result += 2;
    }
    if (three) {
      result += 3;
    }
    return result;
  }

  String findCommonBoxen(String file) throws Exception {
    List<String> lines = Files.readAllLines(Paths.get(file));
    int index = 0;
    while (index < 26) {
      Map<String, Boolean> map = new HashMap<String, Boolean>();
      for (String line : lines) {
        String s = line.substring(0, index) + line.substring(index + 1);
        if (map.containsKey(s)) {
          return s;
        }
        map.put(s, true);
      }
      index += 1;
    }
    throw new Exception("I didn't find the boxes :(");
  }

  int checksum(String file) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get(file));
    int twos = 0;
    int threes = 0;
    for (String line : lines) {
      switch(checkString(line)) {
      case 2: twos += 1;
        break;
      case 3: threes += 1;
        break;
      case 5: twos += 1; threes +=1 ;
        break;
      }
    }
    return twos * threes;
  }

  public static void main(String[] args) throws Exception {
    Checksum cs = new Checksum();
    System.out.println(cs.checksum("dataset"));   
    System.out.println(cs.findCommonBoxen("dataset"));
  }
}

