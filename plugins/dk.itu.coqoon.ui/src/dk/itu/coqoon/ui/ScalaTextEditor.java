package dk.itu.coqoon.ui;

import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.jface.text.source.ISourceViewer;

/* Works around silly eclipse mess: cannot override getAdapter from Scala.
 * This Java class performs the override that BaseCoqEditor needs.
 * For doing so, we declare here the outlinePage member.
 * 
 * All that clearly SUCKS!
 */
public abstract class ScalaTextEditor extends TextEditor {
  private CoqContentOutlinePage outlinePage = null;
  protected abstract CoqContentOutlinePage createOutlinePage();

  @Override
  @SuppressWarnings({"rawtypes", "unchecked"})
  public final Object getAdapter(Class adapter) {
    if (adapter.isAssignableFrom(ISourceViewer.class)) {
      return getSourceViewer();
    } else if (adapter.isAssignableFrom(IContentOutlinePage.class)) {
      if (outlinePage == null && getSourceViewer() != null)
        outlinePage = createOutlinePage();
      return outlinePage;
    } else return super.getAdapter(adapter);
  }

  public ScalaTextEditor() { super(); }
}
