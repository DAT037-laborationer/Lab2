import java.io.*;
import java.util.Scanner;
import java.util.ArrayList;
import java.lang.Runtime;

public class Lab2External {
	public static void main(String[] args) {
		InputStream in = System.in;
		if (args.length > 0) {
			// first argument is a filename
			try {
				in = new FileInputStream(args[0]);
			} catch (FileNotFoundException e) {
				System.err.println("Couldn't open file " + args[0]);
				System.exit(0);
			}
		}

        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            ArrayList<String> cmds = new ArrayList<String>();
            String line = null;
            while ((line = br.readLine()) != null) {
                cmds.add(line);
            }
            String output = pureMain(cmds.toArray(new String[cmds.size()]));
            System.out.println(output);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String pureMain(String[] lines) {
        // open a pipe to the external process, with stdin argument.
        StringBuilder output = new StringBuilder();
        try {
            java.lang.ProcessBuilder processBuilder = new java.lang.ProcessBuilder("./Lab2");
            processBuilder.redirectErrorStream(true);
            Process external = processBuilder.start();
            BufferedWriter externalInput = new BufferedWriter(new OutputStreamWriter(external.getOutputStream()));
            BufferedReader externalOutput = new BufferedReader(new InputStreamReader(external.getInputStream()));
            // feed input into the pipe.
            for (String line : lines) {
                externalInput.write(line + "\n");
            }
            externalInput.close();
            external.waitFor();
            // read output and print it.
            String line = null;
            while ((line = externalOutput.readLine()) != null) {
                //System.out.println(line);
                output.append(line);
                output.append("\n");
            }
            return output.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}