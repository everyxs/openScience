/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Gephi;

import com.itextpdf.text.PageSize;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import org.gephi.filters.api.FilterController;
import org.gephi.graph.api.DirectedGraph;
import org.gephi.graph.api.GraphController;
import org.gephi.graph.api.GraphModel;
import org.gephi.io.exporter.api.ExportController;
import org.gephi.io.exporter.preview.PDFExporter;
import org.gephi.io.exporter.spi.GraphExporter;
import org.gephi.io.generator.plugin.RandomGraph;
import org.gephi.io.importer.api.ImportController;
import org.gephi.io.importer.api.Container;
import org.gephi.io.processor.plugin.DefaultProcessor;
import org.gephi.preview.api.PreviewController;
import org.gephi.preview.api.PreviewModel;
import org.gephi.preview.api.PreviewProperties;
import org.gephi.preview.api.PreviewProperty;
import org.gephi.project.api.ProjectController;
import org.gephi.project.api.Workspace;
import org.openide.util.Lookup;
import org.openide.util.Utilities;
/**
 *
 * @author Xiaoran
 */
public class Visualize {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        //Init a project - and therefore a workspace
        ProjectController pc = Lookup.getDefault().lookup(ProjectController.class);
        pc.newProject();
        Workspace workspace = pc.getCurrentWorkspace();

        //Get controllers and models
        ImportController importController = Lookup.getDefault().lookup(ImportController.class);

        //Import file
        Container container;
        try {
            File file = new File("reproducibility.graphml");
            container = importController.importFile(file);
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
        //Append imported data to GraphAPI
        importController.process(container, new DefaultProcessor(), workspace);
        ExportController ec = Lookup.getDefault().lookup(ExportController.class);
        //Set preview for final visualization
        PreviewModel model = Lookup.getDefault().lookup(PreviewController.class).getModel();
        PreviewProperties prop = model.getProperties();
        prop.putValue(PreviewProperty.EDGE_CURVED, false);
        //Simple PDF export
        try {
           ec.exportFile(new File("reproducibility.pdf"));
        } catch (IOException ex) {
           ex.printStackTrace();
           return;
        }
        
        //plotting 2nd visualization
        pc.newProject();
        workspace = pc.getCurrentWorkspace();
        //Import file
        try {
            File file = new File("openScience.graphml");
            container = importController.importFile(file);
        } catch (Exception ex) {
            ex.printStackTrace();
            return;
        }
        //Set preview for final visualization
        model = Lookup.getDefault().lookup(PreviewController.class).getModel();
        importController.process(container, new DefaultProcessor(), workspace);
        ec = Lookup.getDefault().lookup(ExportController.class);
        prop = model.getProperties();
        prop.putValue(PreviewProperty.EDGE_CURVED, false);
        try {
           ec.exportFile(new File("openScience.pdf"));
        } catch (IOException ex) {
           ex.printStackTrace();
           return;
        }
    }
    
}
