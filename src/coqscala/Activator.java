package coqscala;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class Activator extends AbstractUIPlugin {
  public final static String PLUGIN_ID = "CoqScala";
	
  private Activator single = null;
	
  public Activator () { single = this; }
	
  public void start (BundleContext context) throws Exception {
    super.start(context);
  }

  public void stop (BundleContext context) throws Exception {
    super.stop(context);
  }
    
  public Activator getDefault () { return single; }
}