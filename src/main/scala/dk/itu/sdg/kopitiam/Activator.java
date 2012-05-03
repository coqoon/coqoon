/* (c) 2010-2011 Hannes Mehnert and David Christiansen */

package dk.itu.sdg.kopitiam;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.*;

public class Activator extends AbstractUIPlugin {
  public final static String PLUGIN_ID = "Kopitiam";

  private static Activator single = null;

  public Activator () {
    System.setProperty("file.encoding", "UTF-8");
    System.setProperty("sun.jnu.encoding", "UTF-8");
    single = this;
  }

  public void start (BundleContext context) throws Exception {
    super.start(context);
    System.out.println("starting Kopitiam!");
  }

  public void stop (BundleContext context) throws Exception {
    super.stop(context);
  }

  public static Activator getDefault () { return single; }

  protected void initializeDefaultPreferences(IPreferenceStore store) {
    PreferenceConverter.setDefault(store, "coqSentBg", new RGB(118, 255, 133));
    PreferenceConverter.setDefault(store, "coqSentProcessBg", new RGB(244, 255, 200));
    PreferenceConverter.setDefault(store, "coqKeywordFg", new RGB(127, 6, 101));
  }
}