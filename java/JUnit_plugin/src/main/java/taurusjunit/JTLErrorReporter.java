package taurusjunit;

import java.io.FileOutputStream;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;



public class JTLErrorReporter {
	
	private static final Logger log = Logger.getLogger(JTLReporter.class.getName());
	private String path;
	private DocumentBuilderFactory dcFactory = DocumentBuilderFactory.newInstance();
	private DocumentBuilder dBuilder;
	private Document doc;
	private Element root;
	
	public JTLErrorReporter(String path) {
        log.info("JTLErr file: " + path);
        this.path = path;
        try {
			dBuilder = dcFactory.newDocumentBuilder();
			doc = dBuilder.newDocument();
	        root = doc.createElement("testResults");
	    	root.setAttribute("version", "1.2");
	    	doc.appendChild(root);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			log.severe(e.getMessage());
		}   
    }
	
	
	public void add_sample(Sample sample) {
		log.info("Adding sample to error reporter");
		Element sample_elem = gen_httpSample(sample);
		log.info("sample " + sample_elem.toString());
		root.appendChild(sample_elem);
	}
	
	public Element gen_httpSample(Sample sample) {
		Element sample_elem = doc.createElement("httpSample");
		String data = sample.getResponseMessage();
		
		sample_elem.setAttribute("t", String.valueOf(sample.getElapsed()));
		sample_elem.setAttribute("lt", "0");
		sample_elem.setAttribute("ct", "0");
		sample_elem.setAttribute("ts", String.valueOf(sample.getTimestamp()));
		sample_elem.setAttribute("s", String.valueOf(sample.isSuccessful()));
		sample_elem.setAttribute("lb", sample.getLabel());
		sample_elem.setAttribute("rc", String.valueOf(sample.getResponseCode()));
		sample_elem.setAttribute("rm", "dummy msg");
		sample_elem.setAttribute("tn", sample.getThreadName());
		sample_elem.setAttribute("dt", "text");
		sample_elem.setAttribute("de", "");
		sample_elem.setAttribute("by", String.valueOf(data.length()));
		sample_elem.setAttribute("ng", "1");
		sample_elem.setAttribute("na", "1");
		
		sample_elem.appendChild(gen_resp_header());
		sample_elem.appendChild(gen_req_header());
		sample_elem.appendChild(gen_resp_data(data));
		sample_elem.appendChild(gen_cookies());
		sample_elem.appendChild(gen_method());
		sample_elem.appendChild(gen_queryString());
		sample_elem.appendChild(gen_url());
		return sample_elem;
	}
	
	
	public Element gen_resp_header() {
		Element resp_header = doc.createElement("responseHeader");
		resp_header.setAttribute("class", "java.lang.String");
		return resp_header;
	}
	
	public Element gen_req_header() {
		Element req_header = doc.createElement("requestHeader");
		req_header.setAttribute("class", "java.lang.String");
		return req_header;
	}
	
	public Element gen_resp_data(String data) {
		Element resp_data = doc.createElement("responseData");
		resp_data.setAttribute("class", "java.lang.String");
		resp_data.appendChild(doc.createTextNode(data));
		log.info("Adding resp data to report");
		return resp_data;
	}
	
	public Element gen_cookies() {
		Element cookies = doc.createElement("cookies");
		cookies.setAttribute("class", "java.lang.String");
		return cookies;
	}
	
	public Element gen_method() {
		Element method = doc.createElement("method");
		method.setAttribute("class", "java.lang.String");
		return method;
	}
	
	public Element gen_queryString() {
		Element queryString = doc.createElement("queryString");
		queryString.setAttribute("class", "java.lang.String");
		return queryString;
	}
	
	public Element gen_url() {
		Element url = doc.createElement("java.net.URL");
		url.setAttribute("class", "java.lang.String");
		return url;
	}
	
	public void save(){
		try{
			
		Transformer tr = TransformerFactory.newInstance().newTransformer();
		tr.setOutputProperty(OutputKeys.INDENT, "yes");
		tr.setOutputProperty(OutputKeys.METHOD, "xml");
		tr.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
		tr.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
		tr.transform(new DOMSource(doc), new StreamResult(new FileOutputStream(this.path)));
		}
		catch (Exception e) {
			log.severe(e.getMessage());
		}
	}
	
}
