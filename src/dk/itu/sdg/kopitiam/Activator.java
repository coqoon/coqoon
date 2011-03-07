package dk.itu.sdg.kopitiam;

import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class Activator extends AbstractUIPlugin implements IStartup {
  public final static String PLUGIN_ID = "Kopitiam";
	
  private Activator single = null;
	
  public Activator () { single = this; }
	
  public void start (BundleContext context) throws Exception {
    super.start(context);
  }

  public void stop (BundleContext context) throws Exception {
    super.stop(context);
  }
  
  public Activator getDefault () { return single; }

  public void earlyStartup () {
    System.out.println("earlystartup called");
    DocumentMonitor.init();
    CoqTop.init();
  }
}