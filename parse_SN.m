
%%%%%% STARTING with just CB data!
participant_CB_3_mo_dir = '/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/matlab/CB/3/';
participant_CB_6_mo_dir = '/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/matlab/CB/6/';
participant_CB_9_mo_dir = '/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/matlab/CB/9/';
participant_CB_12_mo_dir = '/Users/tawnytsang/Desktop/Projects/ETdataFiles/infantSibs/CB-SST Data/matlab/CB/12/';
dir_contents=dir(participant_CB_3_mo_dir);

% SN_maps='combinedMaps.mat';
% load(SN_maps);

 points_file = 'CharlieBrown_FA.mat';
 load(points_file);
smooth_rad=20;

% for s=4:length(dir_contents)
     clear face_look partName bouts inside SN_index face_index thisPart xVals yVals xVals_down yVals_down offScreen xp yp offScreen out nonSal sal charlie
     clear SN_and_face SN_not_face face_not_SN on_charlie;
     SN_look=[];
%     partName=dir_contents(s).name
%     thisPart=load(dir_contents(s).name,'-mat');
%     thisPart_data=thisPart.data;

    xVals = thisPart_data(:,1);
    yVals = thisPart_data(:,2);
    offScreen = find(xVals <= 0 | xVals >=1280 | yVals <= 0 | yVals >=1024);
    xVals(offScreen) = NaN; yVals(offScreen)=NaN;

    %%%DOWN SAMPLE x and y coords
%     xVals_down=[]; yVals_down=[];
%     for i=1:6:length(xVals)
%        if i+6 < length(xVals)
%         thisVal= round(nanmean(xVals(i:i+5)));
%         xVals_down=[xVals_down thisVal];
%         
%         thisValY=round(nanmean(yVals(i:i+5)));
%         yVals_down=[yVals_down thisValY];
%        else
%        end
%     end
    
    moviesize = [704 472];
    framesize = [1280 1024];
    adjust = [framesize-moviesize]/2;
    
    lens=7210;

    
    %%%%% SALIENCE CODING! %%%%%%%%
%     realVal = find((adjX(clip1)>0 & adjX(clip1)<1280) & (adjY(clip1)>0 & adjY(clip1)<720));
%     thisframe=round((realVal(i)/(length(AM_fixationAOI)/length(AM_coded_mat))));
     for t = 1:length(xVals)

        xp = xVals(t); %-adjust(1);
        yp = yVals(t); %-adjust(2);
        if (xp<=0 || xp>moviesize(1) || yp<=0 || yp>moviesize(2))
            xp=NaN; yp=NaN;
%         outside = find();
%         adjX=xp; adjx(outside)=NaN; adjY=yp; adjY(outside)=NaN;
        end
          f = ceil(t/6); % match ET with frame
%          f = ceil(1198*t/lens)
%         t/(length(xp)/1198)); 
            if f > 1199
            f=1199;
            end

        frame=combinedMaps{f};
        if isnan(xp) || isnan(yp)
            SN_look=[SN_look; NaN];
        elseif frame(yp,xp)==0
            SN_look=[SN_look; 0];
        else SN_look=[SN_look;1];
        end

        
      %%%%% for FACE ENCODING %%%%%%%%
      
%          f = ceil(t/6); % convert to movie frames at 10fps
%          if f>
%         c = ceil(t/7638/length(points));

          c = ceil(length(points)*t/lens);  %% match ET to frames

%           imagesc(imresize(im,moviesize([2 1])));

        inside = zeros(size(thisPart_data(1,1),1));
        if c > length(points);
            c=length(points);

        end

        for p = 1:length(points(c).p1s)
%           for p = 1:1204;
          x1 = points(c).p1s{p}(2) - smooth_rad;
          x2 = points(c).p2s{p}(2) + smooth_rad;
          y1 = points(c).p1s{p}(1) - smooth_rad;
          y2 = points(c).p2s{p}(1) + smooth_rad;
          
%         xcheck = [xcheck x1]; xcheck2 = [xcheck2 x2];
% 		ycheck = [ycheck y1]; ycheck2=[ycheck2 y2];
        inside(:,p) = xp >= x1 & xp <= x2 & yp >= y1 & yp <= y2 & ~isnan(xp) & ~isnan(yp);
            if isnan(xp)
             face_look{1}(:,t)=NaN; 
            else face_look{1}(:,t) = double(sum(inside,2)>=1);
            end

%          face_look{1}(1:1198)=face_look{1}(2:1199);
%          face_look{1}(1199:end)=[];
         
%          face_look=cell2mat(face_look)';
        end

          
     end
     
%      face_saliency = [SN_look face_look];
     SN_index = find(SN_look==1); face_index = find(face_look{1}==1);
     SN_and_face = intersect(face_index,SN_index);
     SN_not_face=SN_look-face_look{1}';
     face_not_SN=face_look{1}'-SN_look;
%      
    %%%% decode things
    out= length(find(isnan(SN_look)==1));
    nonSal=length(find(SN_look(:)==0));
    sal=length(find(SN_look(:)==1));
    face_count=length(find(face_index(:)==1));
    Sal_face_congruent=length(SN_and_face);
    Sal_over_face=length(find(SN_not_face==1));
    Face_over_sal=length(find(face_not_SN==1));
      
      
      face_bout{1}(1,1)=0;

  for q = 1:size(face_look{1},1)
    for j = 2:size(face_look{1},2)
      if face_look{1}(q,j)==1 & face_look{1}(q,j-1)==1
        face_bout{1}(q,j) = face_bout{1}(q,j-1) + 1;
      else
        face_bout{1}(q,j) = 0;
      end
    end
  end
% end
% 
% %%  now collect them
threshold = -5; % fixations must be more than 100 ms

  for s = 1:size(face_look{1},1)
     ds = diff(face_bout{1}(s,:),1,2);
     bouts{1}{s} = -ds(ds<threshold);
     mbouts(s,1) = nanmean(bouts{1}{s})*100/6;
     lbouts(s,1) = length(bouts{1}{s});
  end



charlie = nanmean(face_look{1},2)*100;
on_charlie = mean(~isnan(face_look{1}))*100;
% % 
% subid
%     
     file_name = fopen('Saliency_face_count.csv','a+');
% % % RUN THE FOLLOWING 2 LINEs ONLY ONCE TO CREATE HEADER NAMES!%
%     fprintf(file_name,'%12s,%3s,%11s,%9s,%15s, %20s, %25s, %20s, %20s\n',...
%    'SubjectID','out','nonSalCount','SalCount N','Percent CB face', ...
%    'CB fixation N','Mean Fixation MS on CB','Congruent','Sal over Face','Face over Sal');
% % % Print data to your file:
% % 
    fprintf(file_name,'%s,%3f,%3f,%3f,%3f,%3f,%3.3f, %3f,%3f,%3f\n',...
         partName,out,nonSal,sal, charlie,...
         lbouts, mbouts,Sal_face_congruent, Sal_over_face, Face_over_sal);
% 
      fclose(file_name);
% 
%    end



  
%   
% % fprintf(fid,'subid,cb,on.cb,ss,on.ss,ss.1st,on.ss.1st,ss.2nd,on.ss.2nd,cb.h1,cb.h2,ss.h1,ss.h2,cb.bout,ss.bout,cb.num.bouts,ss.num.bouts\n')
% 
% for c = 1:length(info)
%   name = info(c).file_name;
%   name = strrep(name,'CMD.txt','');
%   fprintf(fid,'%s,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f,%2.2f\n',...
%     name,charlie(c),on_charlie(c),ss(c),on_ss(c),...
%       ss_1st(c),on_ss_1st(c),ss_2nd(c),on_ss_2nd(c),...
%       halves{1}(c,1),halves{1}(c,2),halves{2}(c,1),halves{2}(c,2),...
%       mbouts(c,1),mbouts(c,2),lbouts(c,1),lbouts(c,2));
% end
% 
    
    
% end

% lens = [6000 6376]; (this is for 50 frames/sec)
% lens = [7210 ]

    
        
%     for t = 1:lens(cond)

          
          
          
          
 %         elseif cond == 2
%    moviesize = [720 480];
%%% SST: 7657

%%% SST start 514




