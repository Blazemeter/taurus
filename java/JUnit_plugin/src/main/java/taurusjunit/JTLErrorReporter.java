package taurusjunit;

import java.io.FileOutputStream;
import java.util.logging.Logger;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import java.io.FileOutputStream;


public class JTLErrorReporter {
	
	private static final Logger log = Logger.getLogger(JTLReporter.class.getName());
	private String path;
	private XMLOutputFactory xof = XMLOutputFactory.newInstance();
	private XMLStreamWriter xsw = null;
	
	//private DocumentBuilderFactory dcFactory = DocumentBuilderFactory.newInstance();
	//private DocumentBuilder dBuilder;
	//private Document doc;
	//private Element root;
	
	public JTLErrorReporter(String path) {
        log.info("JTLErr file: " + path);
        this.path = path;
        try {
        	xsw = xof.createXMLStreamWriter(new FileOutputStream(this.path));
			/*dBuilder = dcFactory.newDocumentBuilder();
			doc = dBuilder.newDocument();
	        root = doc.createElement("testResults");
	    	root.setAttribute("version", "1.2");
	    	doc.appendChild(root);*/
        	xsw.writeStartDocument("UTF-8", "1.0");
        	xsw.writeStartElement("testResults");
        	xsw.writeAttribute("version", "1.2");
        	xsw.flush();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}   
    }
	
	
	//public void add_sample(Sample sample) {
	//	log.info("Adding sample to error reporter");
	//	Element sample_elem = gen_httpSample(sample);
	//	log.info("sample " + sample_elem.toString());
	//	root.appendChild(sample_elem);
	//}
	
	public void add_sample(Sample sample) {
		log.info("Adding sample to error reporter");
		String data = sample.getTrace();
		try {
			xsw.writeStartElement("httpSample");
		xsw.writeAttribute("t", String.valueOf(sample.getElapsed()));
		xsw.writeAttribute("lt", "0");
		xsw.writeAttribute("ct", "0");
		xsw.writeAttribute("ts", String.valueOf(sample.getTimestamp()));
		xsw.writeAttribute("s", String.valueOf(sample.isSuccessful()));
		xsw.writeAttribute("lb", sample.getLabel());
		xsw.writeAttribute("rc", String.valueOf(sample.getResponseCode()));
		xsw.writeAttribute("rm", sample.getResponseMessage());
		xsw.writeAttribute("tn", sample.getThreadName());
		xsw.writeAttribute("dt", "text");
		xsw.writeAttribute("de", "");
		xsw.writeAttribute("by", String.valueOf(data.length()));
		xsw.writeAttribute("ng", "1");
		xsw.writeAttribute("na", "1");
		
		gen_req_header();
		gen_resp_header();
		gen_resp_data(data);
		gen_cookies();
		gen_method();
		gen_queryString();
		gen_url();
		xsw.writeEndElement();
		xsw.flush();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
	}
	
	
	public void gen_resp_header() {
		try {
			xsw.writeStartElement("responseHeader");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_req_header() {
		try {
			xsw.writeStartElement("requestHeader");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_resp_data(String data) {
		try {
			xsw.writeStartElement("responseData");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeCharacters(data);
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_cookies() {
		try {
			xsw.writeStartElement("cookies");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_method() {
		try {
			xsw.writeStartElement("method");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_queryString() {
		try {
			xsw.writeStartElement("queryString");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void gen_url() {
		try {
			xsw.writeStartElement("java.net.URL");
			xsw.writeAttribute("class", "java.lang.String");
			xsw.writeEndElement();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}
		
	}
	
	public void save(){
		try{
			xsw.writeEndElement();
			xsw.writeEndDocument();
			xsw.flush();
			xsw.close();
		}
		catch (Exception e) {
			log.severe(e.getMessage());
		}
	}
	
}
