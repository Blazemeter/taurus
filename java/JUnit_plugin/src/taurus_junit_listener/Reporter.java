package taurus_junit_listener;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Reporter {
	
	private FileWriter out_stream;
	
	public Reporter(String file_name)
	{
		try {
			out_stream = new FileWriter(new File(file_name));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public void write(String s) {
		try {
			out_stream.write(s);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void writeln(String s) {
		try {
			out_stream.write(s+"\n");
			out_stream.flush();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void flush() {
		try {
			out_stream.flush();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public void close(){
		try {
			out_stream.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
