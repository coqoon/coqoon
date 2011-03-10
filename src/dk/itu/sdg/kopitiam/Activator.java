/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class Activator extends AbstractUIPlugin {
  public final static String PLUGIN_ID = "Kopitiam";

  private static Activator single = null;

  public Activator () { single = this; }

  public void start (BundleContext context) throws Exception {
    super.start(context);
  }

  public void stop (BundleContext context) throws Exception {
    super.stop(context);
  }

  public static Activator getDefault () { return single; }
}